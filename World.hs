{-# LANGUAGE StandaloneDeriving #-}
module World where

import Control.Monad

-- GHC imports
import InstEnv
import Module
-- import NameSet
import Unify(tcUnifyTys,BindFlag(BindMe))
import UniqFM
-- import VarSet(mkVarSet)

-- --| A world is just a set of modules that define instances.
-- newtype World = World ModuleSet

-- deriving instance Eq World
-- deriving instance Ord World
-- deriving instance Uniquable World
-- deriving instance Outputable World
-- deriving instance Binary World
-- deriving instance Data World


data 


-- --| A world representing a set of instances. In the absence of holes/signatures,
-- --  a world can be represented, more or less, as a set of module names.
-- data World
--   -- = InitWorld              -- ^ The empty world with no instances.
--   = MergedWorld Worlds     -- ^ A set of merged worlds.
--   | NewWorld Worlds Module -- ^ A new world for a module that extends other
--                            --   worlds and defines at least one instance.


-- | The world of a module that extends other worlds and adds at least one
--   instance.
data Island = Island { wi_exts :: Islands
                     , wi_mod  :: Module
                     , wi_ienv :: IslandInstEnv }

instance Eq Island where
  Island {wi_mod = mod1} == Island {wi_mod = mod2} = mod1 == mod2

instance Ord Island where
  compare Island {wi_mod = mod1}
          Island {wi_mod = mod2} = compare mod1 mod2

-- | Multiple worlds merged together, represented as a map from a Module name
--   to the Island defined by that module. This representation only works
--   in the absence of holes, since each Module name would uniquely determine
--   a set of instances.
type Islands = UniqFM Island

data World = World { w_wimap :: Islands }

-- --| A merged collection of multiple worlds.
-- data MergedWorlds
--   = MergedWorlds { ws_orig  :: [World] -- ^ Original list of worlds.
--                  , ws_canon :: [World] -- ^ Canonical representation of same
--                                        --   set of worlds.

-- | A mapping from Class names to a list of instances for that class.
type IslandInstEnv = UniqFM [ClsInst]

-- | The initial, empty world.
emptyWorld :: World
emptyWorld = World emptyUFM

-- | Merge two worlds.
merge :: World -> World -> Maybe World
merge w1 w2 = do
  -- Make sure they're mergeable.
  guard $ mergeable w1 w2
  -- If a Module appears in two worlds, then we assume that it points to the
  -- exact same Island in both worlds. So to merge two worlds we simply add
  -- the Island maps and take the RHS in case a Mod is mapped by both (which
  -- is what plusUFM does).
  let wimap = plusUFM (w_wimap w1) (w_wimap w2)
  return $ World wimap

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
  return $ World wimap_all


-- | All pairs (xi,xj) of a list xs such that i < j.
pairs :: [a] -> [(a,a)]
pairs xs = [ (x1, x2)
           | (i1, x1) <- zip [0..] xs
           , (i2, x2) <- zip [0..] xs
           , i1 < i2 ]


-- | Check whether the list of worlds is mergeable together, by checking each
--   pair. (Note that mergeability is reflexive and symmetric, so we don't
--   simply check *every* pair in the list.)
mergeableList :: [World] -> Bool
mergeableList ws = and [ mergeable w1 w2 | (w1, w2) <- pairs ws ]

mergeable :: World -> World -> Bool
mergeable (World wimap1) (World wimap2) =
  -- For every Module/Island in w1 and not in w2,
  -- and for every Module/Island in w2 and not in w1,
  -- check that their instance envs are
  -- mergeable with each other.
  and [ mergeableInstEnvs (wi_ienv wi1) (wi_ienv wi2)
      | wi1 <- eltsUFM wimap1minus2
      , wi2 <- eltsUFM wimap2minus1 ]
  where
    -- Map for Modules/Islands in LHS but not RHS.
    wimap1minus2 = minusUFM wimap1 wimap2
    -- Map for Modules/Islands in RHS but not LHS.
    wimap2minus1 = minusUFM wimap2 wimap1

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


-- | Create a new world given a list of worlds to extend, the name of
--   this module, and this module's locally defined instances.
newWorld :: [World] -> Module -> [ClsInst] -> Maybe World
newWorld ws m local_insts = do
  -- First merge the extended worlds into a single parent world.
  pw <- mergeList ws

  -- Organize the list of local instances into an IslandInstEnv.
  -- We assume that this list is internally mergeable.
  let f :: IslandInstEnv -> ClsInst -> IslandInstEnv
      f ienv inst = addToUFM_C (++) ienv (is_cls inst) [inst]
  let local_ienv = foldl f emptyUFM local_insts

  -- Create an Island.
  let island = Island { wi_exts = w_wimap pw
                      , wi_mod  = m
                      , wi_ienv = local_ienv }

  -- Parent world shouldn't have an Island for Module m. If it does,
  -- since we assume all Islands from the same Module are the same, we
  -- don't need to check anything. Just extend the parent world's island
  -- map with this island.
  let wimap_new = addToUFM (w_wimap pw) m island
  return $ World wimap_new


worldInstCount :: World -> Int
worldInstCount (World wimap) = sum [ length insts
                                   | wi <- eltsUFM wimap
                                   , insts <- eltsUFM (wi_ienv wi) ]