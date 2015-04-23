{-# LANGUAGE ScopedTypeVariables #-}
module WorldDagNew where

import Data.GraphViz
  ( graphElemsToDot
  , nonClusteredParams
  , GraphvizParams(..)
  , GlobalAttributes(GraphAttrs, EdgeAttrs)
  , toLabel )
import Data.GraphViz.Attributes.Complete
  ( Attribute(RankDir, FontSize)
  , RankDir(FromBottom))
import Data.GraphViz.Commands
  ( runGraphviz, GraphvizOutput(..), addExtension,  )
import Data.GraphViz.Exception
import Data.List
  ( union )
import Data.Maybe
  ( isJust )
import qualified Data.Map as M
import qualified Data.Set as S

import Module
  ( PackageId, modulePackageId, packageIdString )
import UniqFM
  ( eltsUFM )

import Moduleish
import ReadUtils
import World
import WorldCtx


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

-- type DAG = ([Node], [Edge])
type DAG = (S.Set Node, S.Set Edge)



-- TODO: Get rid of the Moduleish inside WorldOrigin now that we're
--       passing a whole Ctx around?


worldDagOf :: Ctx -> String -> DAG
worldDagOf ctx mod_str = case lookupEntriesMatching ctx mod_str of
  [(_,_,w)] -> worldDag w
  [] -> error $ "! no matches in ctx for module string " ++ mod_str
  es -> error $ "! found " ++ show (length es) ++
                " matches in ctx for module string " ++ mod_str


-- | Construct the DAG of Islands described by the given World.
worldDag :: World -> DAG
worldDag w = (S.unions nodes_list, S.unions edges_list)

  where
    -- The canonical islands of this world.
    islands = S.toList $ canonicalIslands w

    -- recursively compute DAG for each parent
    (nodes_list, edges_list) = unzip $ map islandDag islands


islandDag :: Island -> DAG
islandDag wi = ( S.union my_nodes parent_nodes_merged
               , S.union my_edges parent_edges_merged )


  where
    -- parent islands
    parent_wis = S.toList $ wi_exts wi

    -- recursively compute DAG for each parent
    (parent_nodes, parent_edges) = unzip $ map islandDag parent_wis

    -- merge nodes and edges from parents
    parent_nodes_merged = S.unions parent_nodes
    parent_edges_merged = S.unions parent_edges

    -- Node produced by this world, if it's a new Island.
    -- Edges produced by this world, if it's a new Island. Note that any
    -- edges pointing to something in a cluster need to be redirected to
    -- the cluser node.
    my_nodes = S.singleton (mkN wi, wi_icount wi)
    my_edges = S.fromList $ map mkE parent_wis
    
    mkN wi' = show (wi_mod wi') 
    mkE wi' = (mkN wi, mkN wi', "")


--------------- OUTPUT -------------------

 
graphToDotPng :: FilePath -> DAG -> IO Bool
graphToDotPng fpre (nodes,edges) =
  handle (\(e::GraphvizException) -> return False) $
    addExtension (runGraphviz (graphElemsToDot params nodes' edges')) Png fpre >> return True
  where
    -- params = blankParams { globalAttributes = []
    --                      , clusterBy        = clustBy
    --                      , clusterID        = Num . Int
    --                      , fmtCluster       = clFmt
    --                      , fmtNode          = const []
    --                      , fmtEdge          = const []
    --                      }
    nodes' = S.toList nodes
    edges' = S.toList edges

    params :: GraphvizParams N NL EL () NL
    params = nonClusteredParams
      { isDirected = True
      , globalAttributes = [ GraphAttrs [RankDir FromBottom]
                           , EdgeAttrs [FontSize 8.0] ]
      , fmtNode = \(n, nl) -> [toLabel nl]
      , fmtEdge = \(n1, n2, el) -> [toLabel (mkEdgeLabel n1 n2 el)]
      }

    mkEdgeLabel :: N -> N -> EL -> String
    mkEdgeLabel n1 n2 el
      -- | Just (ps1,_) <- split_mish_str n1
      -- , Just (ps2,_) <- split_mish_str n2
      -- , Just (_,ms)  <- split_mish_str el
      -- , ps1 == ps2 
      --     = ms
      -- | otherwise
      --     = el
      | Just (ps,ms) <- split_mish_str el = ms
      | otherwise                         = el
      where
        split_mish_str s = case span (/= ':') s of
          (ps, ':':ms) -> Just (ps, ms)
          _            -> Nothing
