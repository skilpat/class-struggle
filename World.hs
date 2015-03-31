module World where

import Control.Monad
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Set as S

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


-- --| A world representing a set of instances. In the absence of holes/signatures,
-- --  a world can be represented, more or less, as a set of module names.
-- data World
--   -- = InitWorld              -- ^ The empty world with no instances.
--   = MergedWorld Worlds     -- ^ A set of merged worlds.
--   | NewWorld Worlds Module -- ^ A new world for a module that extends other
--                            --   worlds and defines at least one instance.


-- | The world of a module that extends other worlds and adds at least one
--   instance.
data NewWorld = NewWorld { nw_exts :: MultiWorlds
                         , nw_mod  :: Module
                         , nw_ienv :: WorldInstEnv }

instance Eq NewWorld where
  NewWorld {nw_mod = mod1} == NewWorld {nw_mod = mod2} = mod1 == mod2

instance Ord NewWorld where
  compare NewWorld {nw_mod = mod1} NewWorld {nw_mod = mod2} = compare mod1 mod2

-- | Multiple worlds merged together, represented as a map from a Module name
--   to the NewWorld defined by that module. This representation only works
--   in the absence of holes, since each Module name would uniquely determine
--   a set of instances.
type MultiWorlds = UniqFM NewWorld

data World = World MultiWorlds

-- --| A merged collection of multiple worlds.
-- data MergedWorlds
--   = MergedWorlds { ws_orig  :: [World] -- ^ Original list of worlds.
--                  , ws_canon :: [World] -- ^ Canonical representation of same
--                                        --   set of worlds.

-- | A mapping from Class names to a list of instances for that class.
type WorldInstEnv = UniqFM [ClsInst]

-- | The initial, empty world.
emptyWorld :: World
emptyWorld = World emptyUFM


mergeList :: [World] -> Maybe World
mergeList ws = do


  guard $ mergeableList ws
  return $ World $ S.unions [nws | World nws <- ws]


mergeableList :: [World] -> Bool
mergeableList ws = True

mergeable :: World -> World -> Bool
mergeable (World nwmap1) (World nwmap2) =
  and [ mergeableInstEnvs ienv1 ienv2
      | ienv1 <- nw_ienv $ M.fromJust $ lookupUFM nwmap1
      , ]
  -- for every Module in w1 and not in w2, check that its NewWorld's env is
  -- mergeable with that of every module in w2
  where
    -- Map for Modules in LHS but not RHS.
    nwmap = minusUFM nwmap1 nwmap2


  -- and [ mergeable' nw1 nw2 | nw1 <- S.tonws1
  --                          , nw2 <- nws2 ]

mergeableInstEnvs :: WorldInstEnv -> WorldInstEnv -> Bool
mergeableInstEnvs ienv1 ienv2 =
  and [ mergeableInstLists is1 is2
        | c <- classes
        , let is1 = M.fromMaybe [] $ lookupUFM ienv1 c
        , let is2 = M.fromMaybe [] $ lookupUFM ienv2 c ]
  where
    -- We only need to check for mergeability for instances of classes that
    -- appear in both sides.
    classes = L.intersect (keysUFM ienv1) (keysUFM ienv2)

    -- Just check that every pair of instances is mergeable. Each pair comes
    -- from the same class.
    mergeableInstLists :: [ClsInst] -> [ClsInst] -> Bool
    mergeableInstLists insts1 insts2 =
      and [mergeableInsts i1 i2 | i1 <- insts1, i2 <- insts2]



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




-- --| Merge some worlds together, if possible. The result stores the original list
-- --  as well as a minimal list of worlds that represents the same instances.
-- mergeWorlds :: [World] -> Maybe World
-- mergeWorlds ws0 = go ws0 ws0
--   where
--     go ws0 []     = Just $ Worlds ws0 []
--     go ws0 ((MergedWorld worlds):ws') = do
--       worlds' <- go ws0 


-- insertWorld :: World -> Worlds -> Maybe Worlds
-- insertWorld w (Worlds _ ws) = do



