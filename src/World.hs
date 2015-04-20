{-# LANGUAGE TupleSections #-}
module World where

import Control.Monad
import Data.List
  ( sort )
import Data.Maybe
  ( isNothing, isJust )

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
data Island = Island { wi_exts :: !Islands
                     , wi_mod  :: !Moduleish
                     , wi_ienv :: !IslandInstEnv }

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
                   , w_origin :: !Origin }

-- | Two Worlds are equal if they include the same Moduleishes in their Island
--   maps.
instance Eq World where
  (World wimap1 _) == (World wimap2 _) =
    sort (keysUFM wimap1) == sort (keysUFM wimap2)


-- | A mapping from Class names to a list of instances for that class.
type IslandInstEnv = UniqFM [ClsInst]

-- | The initial, empty world.
emptyWorld :: World
emptyWorld = World emptyUFM (MergedWorlds [])

-- | Merge two worlds.
merge :: World -> World -> Maybe World
merge w1 w2 = do
  -- Make sure they're mergeable.
  guard $ mergeable w1 w2
  -- If a Moduleish appears in two worlds, then we assume that it points to the
  -- exact same Island in both worlds. So to merge two worlds we simply add
  -- the Island maps and take the RHS in case a Mod is mapped by both (which
  -- is what plusUFM does).
  let wimap = plusUFM (w_wimap w1) (w_wimap w2)
  return $ World wimap (MergedWorlds [(w1, Nothing), (w2, Nothing)])

-- | Merge together a list of worlds.
mergeList :: [World] -> Maybe World
mergeList ws = do
  -- Make sure they're mergeable.
  guard $ mergeableList ws
  -- If a Module appears in two worlds, then we assume that it points to the
  -- exact same Island in both worlds. So to merge two worlds we simply add
  -- the Island maps and take the RHS in case a Mod is mapped by both (which
  -- is what plusUFM does).
  let wimaps = map w_wimap ws
  let wimap_all = foldl plusUFM emptyUFM wimaps
  return $ World wimap_all (MergedWorlds $ map (, Nothing) ws)


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
mergeable (World wimap1 _) (World wimap2 _) =
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


-- | Create a new world given a list of worlds to extend, the Moduleish for
--   this module, and this module's locally defined instances.
newWorld :: [World] -> Moduleish -> [ClsInst] -> Maybe World
newWorld ws mish local_insts = newWorld' anno_worlds mish local_insts
  where
    anno_worlds = [(w, Nothing) | w <- ws]


-- | Create a new world given a list of worlds (and Moduleishes that pointed to
--   those worlds) to extend, the Moduleish for
--   this module, and this module's locally defined instances.
newWorldFromImports :: [(Moduleish, World)] -> Moduleish -> [ClsInst] -> Maybe World
newWorldFromImports imps mish local_insts = newWorld' anno_worlds mish local_insts
  where
    anno_worlds = [(w, Just m) | (m,w) <- imps]


newWorld' :: [(World, Maybe Moduleish)] -> Moduleish -> [ClsInst] -> Maybe World
newWorld' anno_worlds mish local_insts = do
  let (ws, annos) = unzip anno_worlds

  -- First merge the extended worlds into a single parent world.
  pw <- mergeList ws

  -- If the list of locally defined instances is empty, then just return
  -- this merged world.
  if null local_insts
    then return $ World (w_wimap pw) (MergedWorlds anno_worlds)
    else do

      -- Organize the list of local instances into an IslandInstEnv.
      -- We assume that this list is internally mergeable.
      let f :: IslandInstEnv -> ClsInst -> IslandInstEnv
          f ienv inst = addToUFM_C (++) ienv (is_cls inst) [inst]
      let local_ienv = foldl f emptyUFM local_insts

      -- Create an Island.
      let island = Island { wi_exts = w_wimap pw
                          , wi_mod  = mish
                          , wi_ienv = local_ienv }

      -- Parent world shouldn't have an Island for Moduleish mish. If it does,
      -- since we assume all Islands from the same Moduleish are the same, we
      -- don't need to check anything. Just extend the parent world's island
      -- map with this island.
      let wimap_new = addToUFM (w_wimap pw) mish island
      return $ World wimap_new (NewWorld anno_worlds island)



lookupIsland :: World -> Moduleish -> Maybe Island
lookupIsland (World wimap _) mish = lookupUFM wimap mish


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
worldInsts (World wimap _) = islandsInsts wimap

islandInstCount :: Island -> Int
islandInstCount wi =
  sum [ length insts
      -- for each unique class's list of instances
      | insts <- eltsUFM (wi_ienv wi) ]

islandsInstCount :: Islands -> Int
islandsInstCount wimap =
  sum [ islandInstCount wi
      | wi <- eltsUFM wimap
      -- Ignore this island if its module is the boot
      -- file of another island's module.
      , not (islandIsOverridden wimap wi) ]

worldInstCount :: World -> Int
worldInstCount (World wimap _) = islandsInstCount wimap

extendedWorlds :: World -> [World]
extendedWorlds w = case w_origin w of
  NewWorld anno_worlds _   -> map fst anno_worlds
  MergedWorlds anno_worlds -> map fst anno_worlds

imports :: World -> [(Moduleish, World)]
imports w = case w_origin w of
  NewWorld anno_worlds _   -> [(m, w) | (w, Just m) <- anno_worlds]
  MergedWorlds anno_worlds -> [(m, w) | (w, Just m) <- anno_worlds]


-- PRINTING ------------------------------------------------------


instance Outputable Island where
  ppr wi = pprMish <+> parens (int (islandInstCount wi))
    where
      mish = wi_mod wi
      pprMish | mish_boot mish = text "--" <+> ppr mish
              | otherwise      = ppr mish



pprIslands :: Islands -> SDoc
pprIslands wimap = sep [ braces listSDoc
                       , parens (int (islandsInstCount wimap)) ]
  where
    islandSDocs = map ppr $ sort $ eltsUFM wimap
    listSDoc = sep $ punctuate (text ", ") $ islandSDocs
