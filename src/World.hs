{-# LANGUAGE TupleSections #-}
module World where

import Control.Applicative
  ( (<|>), liftA2 )
import Control.Monad
import Control.Monad.State
import Data.List
  ( sort, find )
import Data.Maybe
  ( isNothing, isJust, catMaybes )
import qualified Data.Set as S
import qualified Data.HashSet as HS
import Data.Foldable
  ( foldl' )

-- GHC imports
import InstEnv
import Module
import Name
  ( getName )
import Outputable
import Unify
  ( tcUnifyTys, BindFlag(BindMe), tcMatchTys )
import UniqFM
import VarSet
  ( mkVarSet )

import Moduleish



-- | A mapping from Class names to a list of instances for that class.
type IslandInstEnv = UniqFM [ClsInst]

-- | The world of a module that extends other worlds and adds at least one
--   instance.
data Island = Island { wi_exts   :: !(S.Set Island)
                     , wi_mod    :: !Moduleish
                     , wi_ienv   :: !IslandInstEnv
                     , wi_icount :: !Int }

instance Eq Island where
  Island {wi_mod = mod1} == Island {wi_mod = mod2} = mod1 == mod2

instance Ord Island where
  compare Island {wi_mod = mod1}
          Island {wi_mod = mod2} = compare mod1 mod2


-- | Multiple worlds merged together, represented as a map from a Moduleish
--   to the Island defined by that module source file.
type Islands = UniqFM Island


-- | Describes how a World was created: either the merging of some other worlds
--   with no additional instances, or the extension of other worlds with an
--   Island.
data Origin
  = MergedWorlds [(World, Maybe Moduleish)] (Maybe Moduleish) (Maybe IslandClashes)
  | NewWorld [(World, Maybe Moduleish)] Island [NewWorldInconsistency]

-- data WorldInconsistency
--   = FromMerge IslandClashes
--   | FromNew [NewWorldInconsistency]

data NewWorldInconsistency
  = AmongLocals PairwiseInstClashes
  | AmongImports IslandClashes
  | BetweenImportsAndLocals IslandClashes

type IslandClashes = [(Moduleish, ClsInst)]
type PairwiseInstClashes = [(ClsInst, ClsInst)]

data World = World { w_wimap  :: !Islands
                   , w_origin :: !Origin
                   , w_icount :: !Int
                   , w_consis :: !Bool }

-- | Two Worlds are equal if they include the same Moduleishes in their Island
--   maps.
instance Eq World where
  -- UniqFM uses IntMap.keys, which is ordered
  w1 == w2
    | (sizeUFM wimap1) == (sizeUFM wimap2) = keysUFM wimap1 == keysUFM wimap2
    | otherwise = False
    where
      wimap1 = w_wimap w1
      wimap2 = w_wimap w2



-- | The initial, empty world.
emptyWorld :: World
emptyWorld = World emptyUFM (MergedWorlds [] Nothing Nothing) 0 True

-- -- | Merge two worlds.
-- merge :: World -> World -> World
-- merge w1 w2 = World wimap
--                     (MergedWorlds [(w1, Nothing), (w2, Nothing)] Nothing)
--                     (calcIslandsInstCount wimap)
--                     consis
--   where
--     -- Make sure they're mergeable.
--     consis = w_consis w1 && w_consis w2 && mergeable w1 w2
--     -- If a Moduleish appears in two worlds, then we assume that it points to the
--     -- exact same Island in both worlds. So to merge two worlds we simply add
--     -- the Island maps and take the RHS in case a Mod is mapped by both (which
--     -- is what plusUFM does).
--     wimap = plusUFM (w_wimap w1) (w_wimap w2)
  

-- | Merge together a list of worlds.
checkMergeList :: [World] -> Maybe Moduleish -> WorldConsCtx World
checkMergeList ws mb_mish = do
  -- If each one is consistent
  let each_w_consis = and (map w_consis ws)

  -- Check if consistent together, updating cache
  mb_wi_clashes <- checkMergeableListBlame ws

  -- If a Module appears in two worlds, then we assume that it points to the
  -- exact same Island in both worlds. So to merge two worlds we simply add
  -- the Island maps and take the RHS in case a Mod is mapped by both (which
  -- is what plusUFM does).
  let wimaps = map w_wimap ws
  let wimap_all = foldl plusUFM emptyUFM wimaps

  let origin = MergedWorlds (map (, Nothing) ws) mb_mish mb_wi_clashes

  return $! World wimap_all
                  origin
                  (calcIslandsInstCount wimap_all)
                  (each_w_consis && (isNothing mb_wi_clashes))


-- | All pairs (xi,xj) of a list xs such that i < j.
pairs :: [a] -> [(a,a)]
pairs xs = [ (x1, x2)
           | (i1, x1) <- zip ([0..] :: [Int]) xs
           , (i2, x2) <- zip ([0..] :: [Int]) xs
           , i1 < i2 ]


-- | Check whether the list of worlds is mergeable together, by checking each
--   pair. (Note that mergeability is reflexive and symmetric, so we don't
--   simply check *every* pair in the list.)
checkMergeableListBlame :: [World] -> WorldConsCtx (Maybe IslandClashes)
checkMergeableListBlame ws = concatMaybeClashesInCtx checks
  where
    checks = [checkMergeableBlame w1 w2 | (w1, w2) <- pairs ws]

checkMergeableBlame :: World -> World -> WorldConsCtx (Maybe IslandClashes)
checkMergeableBlame = checkMergeableWorldsWithCache compute 
  where
    -- Collect up the island clashes, but first short-circuit if one
    -- world extends the other
    compute :: World -> World -> WorldConsCtx (Maybe IslandClashes)
    compute w1@World{w_wimap = wimap1} w2@World{w_wimap = wimap2}
      | one_extends_other = return Nothing
      | otherwise         = concatMaybeClashesInCtx $! checks
      where
        one_extends_other = worldExtends w1 w2 || worldExtends w2 w1

        -- Map for Modules/Islands in LHS but not RHS.
        wimap1minus2 = minusUFM wimap1 wimap2
        -- Map for Modules/Islands in RHS but not LHS.
        wimap2minus1 = minusUFM wimap2 wimap1

        -- For every Module/Island in w1 and not in w2,
        -- and for every Module/Island in w2 and not in w1,
        -- check that the two Islands are mergeable with each other.
        checks = [ checkMergeableIslandsBlame wi1 wi2
                 | wi1 <- eltsUFM wimap1minus2
                 , wi2 <- eltsUFM wimap2minus1 ]

checkMergeableIslandsBlame :: Island -> Island -> WorldConsCtx (Maybe IslandClashes)
checkMergeableIslandsBlame = checkMergeableIslandsWithCache compute
  where
    compute :: Island -> Island -> Maybe IslandClashes
    compute
      wi1@(Island {wi_mod = mish1, wi_ienv = ienv1})
      wi2@(Island {wi_mod = mish2, wi_ienv = ienv2})
        -- If one is an implementation of the other, check valid impl, blaming
        | similarish mish1 mish2 =
          if mish_boot mish2 then checkIslandImplements wi1 wi2
                             else checkIslandImplements wi2 wi1

        -- Otherwise, check mergeability of instance envs, blaming accordingly.
        | otherwise = fmap (instToIslandClashes mish1 mish2) $! mergeableInstEnvs ienv1 ienv2

-- | Given `m1` and `m2` and pairwise instance clashes, create the island
--   clashes blaming those modules respectively.
instToIslandClashes :: Moduleish -> Moduleish -> PairwiseInstClashes -> IslandClashes
instToIslandClashes m1 m2 inst_clashes = blame1 ++ blame2
  where
    (insts1, insts2) = unzip inst_clashes
    blame1 = map (m1,) insts1
    blame2 = map (m2,) insts2

checkIslandImplements :: Island -> Island -> Maybe IslandClashes
checkIslandImplements wi_impl wi_sig = concatMaybeClashes $! results
  where
    -- Blame the given sig instance.
    sig_inconsis :: ClsInst -> Maybe IslandClashes
    sig_inconsis sig_inst = Just [(wi_mod wi_sig, sig_inst)]

    -- For every instance in wi_sig, check for an identical instance in
    -- wi_impl. Blame sig if not.
    is_impled :: ClsInst -> [ClsInst] -> Maybe IslandClashes
    is_impled sig_inst impl_insts
      | any (identicalClsInstHead sig_inst) impl_insts = Nothing
      | otherwise                                      = sig_inconsis sig_inst

    -- Check that each sig instance is implemented, blaiming sig each time
    results = [ maybe (sig_inconsis sig_inst)
                      (is_impled sig_inst)
                      (lookupUFM (wi_ienv wi_impl) cls_nm)
              | sig_inst <- islandInsts wi_sig
              , let cls_nm = is_cls_nm sig_inst ]

mergeableInstEnvs :: IslandInstEnv -> IslandInstEnv -> Maybe PairwiseInstClashes
mergeableInstEnvs ienv1 ienv2 = foldUFM f Nothing pairs_for_classes
  where
    -- Gather the insts in each side, but only for classes that have
    -- instances in both sides; other classes' instances are fine.
    pairs_for_classes = intersectUFM_C (,) ienv1 ienv2

    -- Folding function: Given two pairs of instances to check and the previous
    -- clashes, concat any new clashes from checking those pairs.
    f :: ([ClsInst], [ClsInst]) -> Maybe PairwiseInstClashes -> Maybe PairwiseInstClashes
    f (is1, is2) mb_clashes = liftA2 (++) mb_clashes $! mergeableInstLists is1 is2


-- Just check that every pair of instances is mergeable. Each pair comes
-- from the same class, hence the use of `mergeableInstsSameClass`.
mergeableInstLists :: [ClsInst] -> [ClsInst] -> Maybe PairwiseInstClashes
mergeableInstLists insts1 insts2 | null inconsis_pairs = Nothing
                                 | otherwise           = Just $! inconsis_pairs
  where
    inconsis_pairs = [ (i1, i2)
                     | i1 <- insts1
                     , i2 <- insts2
                     , not $ mergeableInstsSameClass i1 i2]


-- | Determine whether two instances are mergeable, i.e., non-overlapping,
--   *under the assumption they're for the same class.*
mergeableInstsSameClass :: ClsInst -> ClsInst -> Bool
mergeableInstsSameClass inst1 inst2
  -- If there is an inst param in which inst1 and inst2 have distinct type
  -- names, they're mergeable.
  | instanceCantMatch (is_tcs inst1) (is_tcs inst2)
  = True

  -- They are mergeable if and only if they do *not* unify.
  | otherwise
  =
    -- ASSERT2( tvs1 `disjointVarSet` tvs2,
    --        (ppr tvs1 <+> ppr cls <+> ppr tys1) $$
    --        (ppr tvs2 <+> ppr cls <+> ppr tys2)
    --         )
    -- GHC comments:
    --   Unification will break badly if the variables overlap
    --   They shouldn't because we allocate separate uniques for them
    --   See Note [Template tyvars are fresh]
    case tcUnifyTys (const BindMe) tys1 tys2 of
      Just _  -> False
      Nothing -> True

  where
    cls = is_cls inst1
    tys1 = is_tys inst1
    tys2 = is_tys inst2
    tvs1 = is_tvs inst1
    tvs2 = is_tvs inst2


-- | Determine whether two instances are mergeable, i.e., non-overlapping.
mergeableInsts :: ClsInst -> ClsInst -> Bool
mergeableInsts inst1 inst2
  -- If the classes are different, they're mergeable.
  | is_cls inst1 /= is_cls inst2 = True
  | otherwise                    = mergeableInstsSameClass inst1 inst2


concatMaybeClashes :: [Maybe [a]] -> Maybe [a]
concatMaybeClashes mb_clashes | null clashes = Nothing
                              | otherwise    = Just clashes
  where
    clashes = concat $! catMaybes mb_clashes

concatMaybeClashesInCtx :: [WorldConsCtx (Maybe [a])] -> WorldConsCtx (Maybe [a])
concatMaybeClashesInCtx actions = do
  results <- sequence actions
  return $! concatMaybeClashes results

-- -- | Check whether the given Island is mergeable with the given parent World.
-- --   Not only does this check for mergeability with the parent's Islands, it
-- --   also checks that this Island is a valid implementation of any
-- --   corresponding signature Islands in the parent.
-- checkNewIslandMergeable :: World -> Island -> Maybe IslandClashes
-- checkNewIslandMergeable pw wi = concatMaybeClashes results
--   where
--     results = [ mergeableIslandsBlame pwi wi
--               | pwi <- eltsUFM (w_wimap pw)
--               , pwi /= wi ]
--   -- where
--   --   mish = wi_mod wi
--   --   wi_impls pwi = mish_boot (wi_mod pwi) && not (mish_boot mish)
--   --                  && mish_mod (wi_mod pwi) == mish_mod mish

--     -- -- If this mod implements an earlier one, check that it does indeed.
--     -- is_valid_impl
--     --   -- If this is an impl of some other island, wi_sig, check it.
--     --   | not $ mish_boot mish
--     --   , Just wi_sig <- lookupUFM (w_wimap pw) (mish {mish_boot = True})
--     --     = islandImplements wi wi_sig
--     --   -- Vacuously true.
--     --   | otherwise
--     --     = True


-- -- | Like mergeableList, but returns a pair of Islands to blame if not mergeable.
-- mergeableListBlame :: [World] -> Blame2
-- mergeableListBlame ws = foldl f Nothing (pairs ws)
--   where
--     f :: Blame2 -> (World, World) -> Blame2
--     f r (w1, w2) | isJust r  = r
--                  | otherwise = mergeableBlame w1 w2




-- | Whether `w1` extends `w2`, i.e., has all of the instances of `w2` and
--   more. O(min(n1,n2)).
worldExtends :: World -> World -> Bool
worldExtends w1 w2 = ensureW1MoreInsts && (checkW2ModInW1 || checkW2KeysSubsetW1Keys)
  where
    -- Number of instances of w1 should be greater than that of w2. This is a
    -- prerequisite of w1 extending w2.
    ensureW1MoreInsts = w_icount w1 >= w_icount w2

    -- HEURISTIC: If module that defines w2 appears in islands of w1, then
    -- w1 extends w2.
    checkW2ModInW1 = case moduleOfWorld w2 of
      Just mod_of_w2 -> elemUFM mod_of_w2 $ w_wimap w1
      Nothing        -> False

    -- Key set of islands of w2 should be subset of that of w1; i.e., the mods
    -- seen by w2 should all have been seen by w1.
    checkW2KeysSubsetW1Keys = keySubSet (w_wimap w2) (w_wimap w1)


moduleOfWorld :: World -> Maybe Moduleish
moduleOfWorld w = case w_origin w of
  NewWorld _ (Island {wi_mod = mod_of_w}) _ -> Just mod_of_w
  MergedWorlds _ mb_mod_of_w _              -> mb_mod_of_w

-- | Subset on keys of the two maps. Relies on implementation detail that
--   `keysUFM` returns an ordered list.
keySubSet :: UniqFM a -> UniqFM b -> Bool
keySubSet m1 m2 = ascSubList (keysUFM m1) (keysUFM m2)
  where
    ascSubList [] _ = True
    ascSubList _ [] = False
    ascSubList ks1@(k1:ks1') (k2:ks2') = case compare k1 k2 of
      EQ -> ascSubList ks1' ks2'
      LT -> False
      GT -> ascSubList ks1 ks2'

worldCreatesInconsistency :: World -> Bool
worldCreatesInconsistency w = case w_origin w of
  NewWorld _ _ inconss        -> not $ null inconss
  MergedWorlds _ _ mb_clashes -> isJust mb_clashes


-- NOTE: For all the `newWorld` functions, we assume that the caller has
-- verified that the given local instances have been checked and blamed
-- for inconsistency.

-- | Create a new world given a list of worlds to extend, the Moduleish for
--   this module, and this module's locally defined instances.
checkNewWorld :: [World] -> Moduleish -> ([ClsInst], Maybe PairwiseInstClashes) -> WorldConsCtx World
checkNewWorld ws = checkNewWorld' anno_worlds
  where
    anno_worlds = [(w, Nothing) | w <- ws]


-- | Create a new world given a list of worlds (and Moduleishes that pointed to
--   those worlds) to extend, the Moduleish for
--   this module, and this module's locally defined instances.
checkNewWorldFromImports :: [(Moduleish, World)] -> Moduleish -> ([ClsInst], Maybe PairwiseInstClashes) -> WorldConsCtx World
checkNewWorldFromImports imps = checkNewWorld' anno_worlds
  where
    anno_worlds = [(w, Just m) | (m,w) <- imps]


checkNewWorld' :: [(World, Maybe Moduleish)] -> Moduleish -> ([ClsInst], Maybe PairwiseInstClashes) -> WorldConsCtx World
checkNewWorld' anno_worlds mish (local_insts, mb_local_clashes) = do
    -- First merge the extended worlds into a single parent world.
    let (ws, annos) = unzip anno_worlds
    pw <- checkMergeList ws (Just mish) -- MAYBE CHANGE?

    -- Extract the possible inconsistency among the merged imports
    let (MergedWorlds _ _ mb_imps_clashes) = w_origin pw

    if null local_insts
      then return $! World (w_wimap pw)
                           (MergedWorlds anno_worlds (Just mish) mb_imps_clashes)
                           (w_icount pw)
                           (w_consis pw)
      else do
        -- Organize the list of local instances into an IslandInstEnv.
        -- We assume that this list is internally mergeable; this should
        -- have been checked and blamed at caller, and passed as
        -- mb_local_clashes.
        let f :: IslandInstEnv -> ClsInst -> IslandInstEnv
            f ienv inst = addToUFM_C (++) ienv (is_cls inst) [inst]
        let local_ienv = foldl f emptyUFM local_insts

        -- Create an Island.
        let island = Island { wi_exts   = canonicalIslands pw
                            , wi_mod    = mish
                            , wi_ienv   = local_ienv
                            , wi_icount = length local_insts }

        mb_implocal_clashes <- checkNewIslandMergeableBlame pw island

        -- Parent world shouldn't have an Island for Moduleish mish. If it does,
        -- since we assume all Islands from the same Moduleish are the same, we
        -- don't need to check anything. Just extend the parent world's island
        -- map with this island.
        let wimap_new = addToUFM (w_wimap pw) mish island

        -- Determine new inconsistency
        let nwis = catMaybes $!
              [ toImpsInconsis mb_imps_clashes
              , toLocsInconsis mb_local_clashes
              , toImpsLocsInconsis mb_implocal_clashes ]
        
        return $! World wimap_new
                        (NewWorld anno_worlds island nwis)
                        (calcIslandsInstCount wimap_new)
                        -- parent world is consistent and no new
                        (w_consis pw && null nwis)

  where

    -- Check that each island in the parent world is mergeable with the new
    -- island, using cache.
    checkNewIslandMergeableBlame :: World -> Island -> WorldConsCtx (Maybe IslandClashes)
    checkNewIslandMergeableBlame pw wi = concatMaybeClashesInCtx checks
        where
          checks = [ checkMergeableIslandsBlame pwi wi
                   | pwi <- eltsUFM (w_wimap pw)
                   , pwi /= wi]
        
    toImpsInconsis :: Maybe IslandClashes -> Maybe NewWorldInconsistency
    toImpsInconsis Nothing        = Nothing
    toImpsInconsis (Just clashes) = Just $ AmongImports clashes

    toLocsInconsis :: Maybe PairwiseInstClashes -> Maybe NewWorldInconsistency
    toLocsInconsis Nothing        = Nothing
    toLocsInconsis (Just clashes) = Just $ AmongLocals clashes

    toImpsLocsInconsis :: Maybe IslandClashes -> Maybe NewWorldInconsistency
    toImpsLocsInconsis Nothing        = Nothing
    toImpsLocsInconsis (Just clashes) = Just $ BetweenImportsAndLocals clashes


localInconsistentInstancesBlame :: [ClsInst] -> Maybe PairwiseInstClashes
localInconsistentInstancesBlame insts | null results = Nothing
                                      | otherwise    = Just results
  where
    results = filter (\(i1, i2) -> not (mergeableInsts i1 i2)) $ pairs insts


lookupIsland :: World -> Moduleish -> Maybe Island
lookupIsland w mish = lookupUFM (w_wimap w) mish


-- | If the given island comes from a boot file, get the island corresponding
--   to its implementation, if it exists. Otherwise, Nothing. When this
--   evaluates to Nothing then the given Island indeed contributes to the total
--   instances known to a containing world.
islandImplementation :: Islands -> Island -> Maybe Island
islandImplementation wimap wi
  | mish_boot mish = lookupUFM wimap $ mish { mish_boot = False }
  | otherwise      = Nothing
  where
    mish = wi_mod wi

islandIsOverridden :: Islands -> Island -> Bool
islandIsOverridden wimap wi = isJust $ islandImplementation wimap wi

islandInsts :: Island -> [ClsInst]
islandInsts wi = concat $ eltsUFM (wi_ienv wi)

islandsInsts :: Islands -> [ClsInst]
islandsInsts wimap =
  concat [ islandInsts wi
         | wi <- eltsUFM wimap
         -- Ignore this island if its module is the boot
         -- file of another island's module.
         , not (islandIsOverridden wimap wi) ]

worldIslandCount :: World -> Int
worldIslandCount w = sizeUFM $ w_wimap w

worldInsts :: World -> [ClsInst]
worldInsts w = islandsInsts $ w_wimap w

islandInstCount :: Island -> Int
islandInstCount = wi_icount

calcIslandInstCount :: Island -> Int
calcIslandInstCount wi =
  sum [ length insts
      -- for each unique class's list of instances
      | insts <- eltsUFM (wi_ienv wi) ]

calcIslandsInstCount :: Islands -> Int
calcIslandsInstCount wimap =
  sum [ islandInstCount wi
      | wi <- eltsUFM wimap
      -- Ignore this island if its module is the boot
      -- file of another island's module.
      , not (islandIsOverridden wimap wi) ]

worldInstCount :: World -> Int
worldInstCount = w_icount

calcWorldInstCount :: World -> Int
calcWorldInstCount w = calcIslandsInstCount $ w_wimap w

extendedWorlds :: World -> [World]
extendedWorlds w = case w_origin w of
  NewWorld anno_worlds _ _     -> map fst anno_worlds
  MergedWorlds anno_worlds _ _ -> map fst anno_worlds

imports :: World -> [(Moduleish, World)]
imports w = case w_origin w of
  NewWorld anno_worlds _ _     -> [(m, w) | (w, Just m) <- anno_worlds]
  MergedWorlds anno_worlds _ _ -> [(m, w) | (w, Just m) <- anno_worlds]


coveredPkgs :: World -> S.Set PackageId
coveredPkgs w =
  S.fromList [ modulePackageId (mish_mod (wi_mod wi))
               | wi <- eltsUFM (w_wimap w) ]
  
canonicalIslands :: World -> S.Set Island
canonicalIslands w = case w_origin w of
  NewWorld _ wi _ -> S.singleton $ wi
  MergedWorlds anno_ws _ _ ->
    S.unions [ canonicalIslands w' | (w', _) <- anno_ws ]

-- PRINTING ------------------------------------------------------


instance Outputable Island where
  ppr wi = pprMish <+> parens (int (islandInstCount wi))
    where
      mish = wi_mod wi
      pprMish | mish_boot mish = text "--" <+> ppr mish
              | otherwise      = ppr mish



pprIslands :: Islands -> SDoc
pprIslands wimap = sep [ braces listSDoc
                       , parens (int (calcIslandsInstCount wimap)) ]
  where
    islandSDocs = map ppr $ sort $ eltsUFM wimap
    listSDoc = sep $ punctuate (text ", ") $ islandSDocs


---------------------------------------------------------------


identicalClsInstHead :: ClsInst -> ClsInst -> Bool
-- ^ True when when the instance heads are the same
-- e.g.  both are   Eq [(a,b)]
-- Used for overriding in GHCi
-- Obviously should be insenstive to alpha-renaming
identicalClsInstHead (ClsInst { is_cls_nm = cls_nm1, is_tcs = rough1, is_tvs = tvs1, is_tys = tys1 })
                     (ClsInst { is_cls_nm = cls_nm2, is_tcs = rough2, is_tvs = tvs2, is_tys = tys2 })
  =  cls_nm1 == cls_nm2
  && not (instanceCantMatch rough1 rough2)  -- Fast check for no match, uses the "rough match" fields
  && isJust (tcMatchTys (mkVarSet tvs1) tys1 tys2)
  && isJust (tcMatchTys (mkVarSet tvs2) tys2 tys1)


------------------------



type WorldConsCacheKey = (Moduleish, Moduleish)
type WorldConsCache = HS.Set WorldConsCacheKey

worldConsCacheContains :: World -> World -> WorldConsCache -> Bool
worldConsCacheContains w1 w2 cache = case toRootCacheKey w1 w2 of
  Just key -> HS.member key cache
  Nothing  -> False

worldConsCacheContainsIslands :: Island -> Island -> WorldConsCache -> Bool
worldConsCacheContainsIslands wi1 wi2 cache = HS.member key cache
  where
    key = toIslandCacheKey wi1 wi2

worldConsCacheAdd :: World -> World -> WorldConsCache -> WorldConsCache
worldConsCacheAdd w1 w2 cache = --HS.union cache $! toAllCacheableKeys w1 w2
  case toRootCacheKey w1 w2 of
    Just key -> HS.insert key cache
    Nothing  -> cache

worldConsCacheAddIslands :: Island -> Island -> WorldConsCache -> WorldConsCache
worldConsCacheAddIslands wi1 wi2 = HS.insert $! toIslandCacheKey wi1 wi2

-- -- | Return the cache containing all keys derived from mergeable worlds
-- --   `w1` and `w2`. This includes `(mod(w1), mod(w2))` (the root cache key)
-- --   but also every pair of modules from islands in the two worlds, resp.
-- toAllCacheableKeys :: World -> World -> WorldConsCache
-- toAllCacheableKeys w1 w2 = foldl' (flip HS.insert) root_cache island_keys
--   where
--     -- The root cache for the root cache key of (mod(w1), mod(w2))
--     root_cache = case toRootCacheKey w1 w2 of
--       Just key -> HS.singleton key
--       Nothing -> HS.empty

--     -- Pairs of all modules found among islands in w1 and w2, resp. These are
--     -- all the keys that can be added to the cache by analyzing islands.
--     wis1 = eltsUFM $ w_wimap w1
--     wis2 = eltsUFM $ w_wimap w2
--     island_keys = [ toIslandCacheKey wi1 wi2
--                   | wi1 <- wis1
--                   , wi2 <- wis2 ]

toIslandCacheKey :: Island -> Island -> WorldConsCacheKey
toIslandCacheKey wi1 wi2 = (wi_mod wi1, wi_mod wi2)

-- | Get the cache key for `(mod(w1), mod(w2))`. This is the root cache key
--   for the two (mergeable) worlds because it doesn't derive all the
--   additional keys from the islands known to each world. It will exist
--   whenever both worlds have immediately identifiable modules that created
--   them.
toRootCacheKey :: World -> World -> Maybe WorldConsCacheKey
toRootCacheKey w1 w2 = case (moduleOfWorld w1, moduleOfWorld w2) of
  (Just mish1, Just mish2) | mish1 <= mish2 -> Just (mish1, mish2)
                           | otherwise      -> Just (mish2, mish1)
  _ -> Nothing


type WorldConsCtx = State WorldConsCache

-- | Given a function to compute mergeability between worlds and return blame,
--   return a new function on two worlds that will first check the cache on
--   those worlds; use the compute function if not present; update the cache
--   if success; and return the compute function's blame.
checkMergeableWorldsWithCache :: (World -> World -> WorldConsCtx (Maybe a)) -> World -> World -> WorldConsCtx (Maybe a)
checkMergeableWorldsWithCache compute_mergeable_action w1 w2 = do
  cache <- get
  if worldConsCacheContains w1 w2 cache
    then return Nothing
    else do
      -- Run the compute action
      mb_inconsis <- compute_mergeable_action w1 w2
      case mb_inconsis of
        Just _  -> return mb_inconsis
        Nothing -> do
          modify $! worldConsCacheAdd w1 w2
          return Nothing

-- | Given two worlds and a blame for failure (as a `Maybe`), add these two
--   worlds to the cache when the blame is empty (i.e., it was a success),
--   and in all cases return the blame.
checkMergeableIslandsWithCache :: (Island -> Island -> Maybe a) -> Island -> Island -> WorldConsCtx (Maybe a)
checkMergeableIslandsWithCache compute_mergeable wi1 wi2 = do
  cache <- get
  if worldConsCacheContainsIslands wi1 wi2 cache
    then return Nothing
    else case compute_mergeable wi1 wi2 of
      Just blame -> return $ Just blame
      Nothing    -> do
        modify $! worldConsCacheAddIslands wi1 wi2
        return Nothing

