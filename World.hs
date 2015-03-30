module World where

import Control.Monad
import qualified Data.Set as S
import Module
import NameSet

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


data World = World MultiWorlds

--| The world of a module that extends other worlds and adds at least one
--  instance.
data NewWorld = NewWorld { nw_exts  :: MultiWorlds
                         , nw_mod   :: Module
                         , nw_insts ::  }

--| Multiple worlds merged together. By looking only
--  at the Module component of each constituent NewWorld, we have all the
--  constituent modules of the world. The collection of constituent NewWorlds
--  is ordered by their corresponding Module components.
type MultiWorlds = MultiWorlds (S.Set NewWorld)

-- --| A merged collection of multiple worlds.
-- data MergedWorlds
--   = MergedWorlds { ws_orig  :: [World] -- ^ Original list of worlds.
--                  , ws_canon :: [World] -- ^ Canonical representation of same
--                                        --   set of worlds.

--| The initial, empty world.
emptyWorld :: World
emptyWorld = World []


mergeWorlds :: [World] -> Maybe World
mergeWorlds ws = do
  guard $ mergeable ws
  return $ S.unions [nws | World nws <- ws]


mergeable :: [World] -> Bool
mergeable ws = True

mergeable2 :: World -> World -> Bool
mergeable2 w1 w2 = 


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



