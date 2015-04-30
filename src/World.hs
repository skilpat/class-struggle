{-# LANGUAGE TupleSections #-}
module World where

import Control.Monad
import Data.List
  ( sort )
import Data.Maybe
  ( isNothing, isJust )
import qualified Data.Set as S

-- GHC imports
import InstEnv
import Module
import Outputable
import Unify
  ( tcUnifyTys, BindFlag(BindMe) )
import UniqFM

import Moduleish



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
  = MergedWorlds [(World, Maybe Moduleish)]
  | NewWorld [(World, Maybe Moduleish)] Island


data World = World { w_wimap  :: !Islands
                   , w_origin :: !Origin
                   , w_icount :: !Int
                   , w_consis :: !Bool }

-- | Two Worlds are equal if they include the same Moduleishes in their Island
--   maps.
instance Eq World where
  w1 == w2 =
    sort (keysUFM (w_wimap w1)) == sort (keysUFM (w_wimap w2))


-- | A mapping from Class names to a list of instances for that class.
type IslandInstEnv = UniqFM [ClsInst]





-- | The initial, empty world.
emptyWorld :: World
emptyWorld = World emptyUFM (MergedWorlds []) 0 True

-- | Merge two worlds.
merge :: World -> World -> World
merge w1 w2 = World wimap
                    (MergedWorlds [(w1, Nothing), (w2, Nothing)])
                    (calcIslandsInstCount wimap)
                    consis
  where
    -- Make sure they're mergeable.
    consis = w_consis w1 && w_consis w2 && mergeable w1 w2
    -- If a Moduleish appears in two worlds, then we assume that it points to the
    -- exact same Island in both worlds. So to merge two worlds we simply add
    -- the Island maps and take the RHS in case a Mod is mapped by both (which
    -- is what plusUFM does).
    wimap = plusUFM (w_wimap w1) (w_wimap w2)
  

-- | Merge together a list of worlds.
mergeList :: [World] -> World
mergeList ws = World wimap_all
                     (MergedWorlds $ map (, Nothing) ws)
                     (calcIslandsInstCount wimap_all)
                     consis
  where
    -- Make sure they're mergeable.
    consis = and (map w_consis ws) && mergeableList ws
    -- If a Module appears in two worlds, then we assume that it points to the
    -- exact same Island in both worlds. So to merge two worlds we simply add
    -- the Island maps and take the RHS in case a Mod is mapped by both (which
    -- is what plusUFM does).
    wimaps = map w_wimap ws
    wimap_all = foldl plusUFM emptyUFM wimaps


-- | All pairs (xi,xj) of a list xs such that i < j.
pairs :: [a] -> [(a,a)]
pairs xs = [ (x1, x2)
           | (i1, x1) <- zip ([0..] :: [Int]) xs
           , (i2, x2) <- zip ([0..] :: [Int]) xs
           , i1 < i2 ]


-- | Check whether the list of worlds is mergeable together, by checking each
--   pair. (Note that mergeability is reflexive and symmetric, so we don't
--   simply check *every* pair in the list.)
mergeableList :: [World] -> Bool
mergeableList ws = and [ mergeable w1 w2 | (w1, w2) <- pairs ws ]

mergeable :: World -> World -> Bool
mergeable World{w_wimap = wimap1} World{w_wimap = wimap2} =
  -- For every Module/Island in w1 and not in w2,
  -- and for every Module/Island in w2 and not in w1,
  -- check that the two Islands are mergeable with each other.
  and [ mergeableIslands wi1 wi2
      | wi1 <- eltsUFM wimap1minus2
      , wi2 <- eltsUFM wimap2minus1 ]
  where
    -- Map for Modules/Islands in LHS but not RHS.
    wimap1minus2 = minusUFM wimap1 wimap2
    -- Map for Modules/Islands in RHS but not LHS.
    wimap2minus1 = minusUFM wimap2 wimap1

    mergeableIslands :: Island -> Island -> Bool
    mergeableIslands (Island {wi_mod = mish1, wi_ienv = ienv1})
                     (Island {wi_mod = mish2, wi_ienv = ienv2})
      | similarish mish1 mish2 = True     -- guaranteed by the type checker!
      | otherwise              = mergeableInstEnvs ienv1 ienv2


    mergeableInstEnvs :: IslandInstEnv -> IslandInstEnv -> Bool
    mergeableInstEnvs ienv1 ienv2 =
      and [ mergeableInstLists is1 is2
            -- Gather the insts in each side, but only for classes that have
            -- instances in both sides; other classes' instances are fine.
            | (is1, is2) <- eltsUFM $ intersectUFM_C (,) ienv1 ienv2 ]

    -- Just check that every pair of instances is mergeable. Each pair comes
    -- from the same class.
    mergeableInstLists :: [ClsInst] -> [ClsInst] -> Bool
    mergeableInstLists insts1 insts2 =
      and [mergeableInsts i1 i2 | i1 <- insts1, i2 <- insts2]


-- | Determine whether two instances are mergeable, i.e., non-overlapping.
mergeableInsts :: ClsInst -> ClsInst -> Bool
mergeableInsts inst1 inst2
  -- If the classes are different, they're mergeable.
  | is_cls inst1 /= is_cls inst2
  = True
  
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

-- newIslandMergeable :: World -> Island -> Bool
-- newIslandMergeable pw wi
--   -- If this mod implements an earlier one, check that it does indeed.
--   | not $ mish_boot (wi_mod wi)
--   , Just wi_boot <- lookupUFM (w_wimap pw) (mish {mish_boot = True})
--     = 
--   where
--     mish = wi_mod wi


-- NOTE: For all the `newWorld` functions, we assume that GHC typechecking has
-- guaranteed that the freshly defined instances in the new Island do not
-- overlap with anything in the parent worlds.

-- | Create a new world given a list of worlds to extend, the Moduleish for
--   this module, and this module's locally defined instances.
newWorld :: [World] -> Moduleish -> [ClsInst] -> World
newWorld ws mish local_insts = newWorld' anno_worlds mish local_insts
  where
    anno_worlds = [(w, Nothing) | w <- ws]


-- | Create a new world given a list of worlds (and Moduleishes that pointed to
--   those worlds) to extend, the Moduleish for
--   this module, and this module's locally defined instances.
newWorldFromImports :: [(Moduleish, World)] -> Moduleish -> [ClsInst] -> World
newWorldFromImports imps mish local_insts = newWorld' anno_worlds mish local_insts
  where
    anno_worlds = [(w, Just m) | (m,w) <- imps]


newWorld' :: [(World, Maybe Moduleish)] -> Moduleish -> [ClsInst] -> World
newWorld' anno_worlds mish local_insts
  | null local_insts = World (w_wimap pw)
                             (MergedWorlds anno_worlds)
                             (w_icount pw)
                             (w_consis pw)
  | otherwise = let

      -- Organize the list of local instances into an IslandInstEnv.
      -- We assume that this list is internally mergeable.
      f :: IslandInstEnv -> ClsInst -> IslandInstEnv
      f ienv inst = addToUFM_C (++) ienv (is_cls inst) [inst]
      local_ienv = foldl f emptyUFM local_insts

      -- Create an Island.
      island = Island { wi_exts   = canonicalIslands pw
                      , wi_mod    = mish
                      , wi_ienv   = local_ienv
                      , wi_icount = (length local_insts) }

      -- Parent world shouldn't have an Island for Moduleish mish. If it does,
      -- since we assume all Islands from the same Moduleish are the same, we
      -- don't need to check anything. Just extend the parent world's island
      -- map with this island.
      wimap_new = addToUFM (w_wimap pw) mish island
      in
        World wimap_new
              (NewWorld anno_worlds island)
              (calcIslandsInstCount wimap_new)
              (w_consis pw)

  where
    -- First merge the extended worlds into a single parent world.
    (ws, annos) = unzip anno_worlds
    pw = mergeList ws



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
  NewWorld anno_worlds _   -> map fst anno_worlds
  MergedWorlds anno_worlds -> map fst anno_worlds

imports :: World -> [(Moduleish, World)]
imports w = case w_origin w of
  NewWorld anno_worlds _   -> [(m, w) | (w, Just m) <- anno_worlds]
  MergedWorlds anno_worlds -> [(m, w) | (w, Just m) <- anno_worlds]


coveredPkgs :: World -> S.Set PackageId
coveredPkgs w =
  S.fromList [ modulePackageId (mish_mod (wi_mod wi))
               | wi <- eltsUFM (w_wimap w) ]
  
canonicalIslands :: World -> S.Set Island
canonicalIslands w = case w_origin w of
  NewWorld _ wi  -> S.singleton $ wi
  MergedWorlds anno_ws ->
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
