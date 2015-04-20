module WorldDag where

-- import Data.GraphViz
--   ( graphElemtsToDot )
-- import Data.GraphViz.Attributes.Complete
--   ( Attribute(RankDir, Splines, FontName)
--   , RankDir(FromLeft)
--   , EdgeType(SplineEdges) )
import Data.List
  ( union )
import Data.Maybe
  ( isJust )
import qualified Data.Map as M
import qualified Data.Set as S

import UniqFM
  ( eltsUFM )

import Moduleish
import World


-- FIRST ATTEMPT:
-- * Each node is an island, i.e., a World that is a NewWorld.
-- * A world corresponds to a set of nodes.
-- * Each edge (i,j) goes from an island to an island of a world that it
--   directly extends.
-- * There's no way to pick out the individual worlds (i.e., sets of nodes)
--   that an island directly extends. Instead, you can only see the total set
--   of islands that a world extends, i.e., the world gotten by merging all
--   the directly extended worlds.
-- * Each node is represented as the Moduleish of the Island, as a String.
-- * Each node is labeled with the number of instances defined by that Island.
-- * Each edge is labeled with the name of the imported module.


type N  = String -- Moduleish of Island as String
type NL = Int    -- Instance count of Island
type EL = String -- Moduleish that was imported

type Node = (N, NL)
type Edge = (N, N, EL)

type MW = (Moduleish, World)

-- | Construct the DAG of Islands described by the given World.
worldDag :: Moduleish -> World -> ([Node], [Edge])
worldDag mish w = worldDagExcept mish w []

worldDagExcept :: Moduleish -> World -> [MW] -> ([Node], [Edge])
worldDagExcept mish w skip_mw = worldDag' mish w ci
  where
    ci = ClusterInfo skip_map imp_nodes

    skip_map = M.fromList [ (show m', worldAllNodes w')
                          | (m', w') <- skip_mw ]
    
    -- | Map a given imported Moduleish and its World to a list of nodes to
    --   which the present node should point. If the imported Moduleish matches
    --   one that we should skip, send it to a cluster for that node. Otherwise,
    --   if all the imported World's canonical nodes are contained in the
    --   canonical nodes of a skipped world, send it to a cluster for that
    --   skipped world.
    imp_nodes (imp_m, imp_w)
      | M.member (show imp_m) skip_map = [clusterN (show imp_m)]
      | otherwise = case M.foldlWithKey f Nothing skip_map of
        Just n  -> [clusterN n]
        Nothing -> S.toList imp_w_ns
      
      where
        imp_w_ns = worldCanonicalNodes imp_w
        
        f :: Maybe N -> N -> S.Set N -> Maybe N
        f res next_m next_ns
          | isJust res                    = res
          | S.isSubsetOf imp_w_ns next_ns = Just $ clusterN next_m
          | otherwise                     = Nothing


data ClusterInfo =
  ClusterInfo { ci_skip_map  :: M.Map N (S.Set N)
              , ci_imp_nodes :: MW -> [N] }


worldDag' :: Moduleish -> World -> ClusterInfo -> ([Node], [Edge])
worldDag' mish w ci = (nodes, edges)
  where
    ClusterInfo { ci_skip_map = skip_map, ci_imp_nodes = imp_nodes } = ci
    me = show mish

    -- recursively compute DAG for each parent
    (parent_nodes, parent_edges) =
      unzip $ [ worldDag' pm pw ci | (pm, pw) <- imports w ]

    -- Merge nodes and edges from parents
    parent_nodes_merged = foldl union [] parent_nodes
    parent_edges_merged = foldl union [] parent_edges

    -- merge their nodes and edges
    (nodes, edges)
      -- In case this is a world to skip, return only this node.
      | M.member me skip_map = ( [clusterNode mish w], [] )

      -- Otherwise, recursively merge in DAGs of parents.
      | otherwise = ( my_nodes `union` parent_nodes_merged
                    , my_edges `union` parent_edges_merged )
        where
          -- Node produced by this world, if it's a new Island.
          -- Edges produced by this world, if it's a new Island. Note that any
          -- edges pointing to something in a cluster need to be redirected to
          -- the cluser node.
          (my_nodes, my_edges) = case w_origin w of
            MergedWorlds _ -> ([], [])
            NewWorld _ wi  ->
              ( [(show (wi_mod wi), islandInstCount wi)]
              , [ (me, n, (show imp_m))
                | (imp_m, imp_w) <- imports w
                , n <- imp_nodes (imp_m, imp_w) ] )

clusterN :: N -> N
clusterN n = "(" ++ n ++ ")"

clusterNode :: Moduleish -> World -> Node
clusterNode mish w = (clusterN (show mish), worldInstCount w)

-- | The canonical set of nodes corresponding to this World, i.e., the smallest
--   set of Islands whose total reachability set includes every Island in this
--   World.
worldCanonicalNodes :: World -> S.Set N
worldCanonicalNodes w = case w_origin w of
  NewWorld _ wi  -> S.singleton $ show (wi_mod wi)
  MergedWorlds anno_ws ->
    S.unions [ worldCanonicalNodes w' | (w', _) <- anno_ws ]

worldAllNodes :: World -> S.Set N
worldAllNodes (World wimap _) =
  S.fromList [ show (wi_mod wi) | wi <- eltsUFM wimap ]


