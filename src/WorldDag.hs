{-# LANGUAGE ScopedTypeVariables #-}
module WorldDag where

import Control.Monad
  ( mapAndUnzipM )
import Control.Monad.State.Strict
  ( State, evalState, get, modify )
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

type DagCache = M.Map Moduleish DAG

-- TODO: Get rid of the Moduleish inside WorldOrigin now that we're
--       passing a whole Ctx around?


-- | Construct the DAG of Islands described by the given World.
worldDag :: Ctx -> String -> DAG
worldDag ctx mod_str = worldDagExceptPkgs ctx mod_str []


worldDagExceptExtPkgs :: Ctx -> String -> DAG
worldDagExceptExtPkgs ctx mod_str =
  evalState (worldDagInner ci (mish0, w0)) M.empty
  where
    -- Make sure that the requested module matches a single one in ctx
    (mish0, w0, pid0) = case lookupEntriesMatching ctx mod_str of
      [(m,_,w)] -> (m, w, modulePackageId (mish_mod m))
      [] -> error $ "! no matches in ctx for module string " ++ mod_str
      es -> error $ "! found " ++ show (length es) ++
                    " matches in ctx for module string " ++ mod_str
    -- All pids in ctx minus this module's
    skip_pkgs = map packageIdString $ S.toList $ S.delete pid0 (ctx_pkgs ctx)

    -- Make the clustering object
    ci = makeClusterInfo ctx w0 skip_pkgs


worldDagExceptPkgs :: Ctx -> String -> [String] -> DAG
worldDagExceptPkgs ctx mod_str skip_pkgs =
  evalState (worldDagInner ci (mish0, w0)) M.empty
  where
    -- Make sure that the requested module matches a single one in ctx
    (mish0, w0) = case lookupEntriesMatching ctx mod_str of
      [(m,_,w)] -> (m, w)
      [] -> error $ "! no matches in ctx for module string " ++ mod_str
      es -> error $ "! found " ++ show (length es) ++
                    " matches in ctx for module string " ++ mod_str

    -- Make the clustering object
    ci = makeClusterInfo ctx w0 skip_pkgs


data ClusterInfo =
  ClusterInfo { ci_skip_pids :: S.Set PackageId
              , ci_cluster   :: Moduleish -> Maybe Node
              , ci_imp_nodes :: Moduleish -> Moduleish -> World -> S.Set N }


makeClusterInfo :: Ctx -> World -> [String] -> ClusterInfo
makeClusterInfo ctx w0 skip_pkgs = ClusterInfo skip_pids cluster imp_nodes
  where
    -- Does the given PackageId match any of the skipped packages.
    should_skip pid =
      any (\s -> packageIdString pid == s || pkgIdName pid == s) skip_pkgs

    -- Get all pkg ids in ctx that match the search strings
    skip_pids :: S.Set PackageId
    skip_pids = S.filter should_skip $ coveredPkgs w0

    cluster :: Moduleish -> Maybe Node
    cluster mish
      | S.member pid skip_pids, Just w <- lookupPkgWorld ctx pid
                  = Just $ clusterNode mish w
      | otherwise = Nothing
      where
        pid = modulePackageId (mish_mod mish)

    imp_nodes :: Moduleish -> Moduleish -> World -> S.Set N
    imp_nodes mish imp_m imp_w = case cluster imp_m of
      -- If the imported mod should be clustered and its the same package,
      -- then do nothing.
      Just _     | mishPkgStr mish == mishPkgStr imp_m -> S.empty
      -- If the imported mod should be clustered and its a different package,
      -- need an edge to just the cluster's node. 
      Just (n,_) | otherwise -> S.singleton n
      -- If the imported mod should *not* be clustered, then need an edge to
      -- each of the nodes in its world's canonical set.
      Nothing -> worldCanonicalNodes imp_w


-- IF CLUSTER ME: Then create cluster node and no edges.
-- OTHERWISE:
--    1. For each of my imported mods/worlds, recursively compute DAGS.
--    2. If I create a new node, then:
--      2.1. Add my node.
--      2.2. Add edges for each of my imports.
worldDagInner :: ClusterInfo -> (Moduleish, World) -> State DagCache DAG
worldDagInner ci (mish, w) = do
  -- Check the cache for a DAG for this module.
  cache <- get
  case M.lookup mish cache of
    Just dag -> return dag
    Nothing  -> do
      -- If it doesn't exist, compute it and add it to cache.
      dag <- worldDagInner' (mish, w)
      modify $ \cache -> M.insert mish dag cache
      return dag

  where
    -- Unpack cluster functions
    ClusterInfo { ci_cluster = cluster, ci_imp_nodes = imp_nodes } = ci

    -- Main code for computing DAG
    worldDagInner' :: (Moduleish, World) -> State DagCache DAG
    worldDagInner' (mish, w)
      | Just cluster_node <- cluster mish =
          return (S.singleton cluster_node, S.empty)
      | otherwise = do
          -- recursively compute DAG for each parent
          (parent_nodes, parent_edges) <- mapAndUnzipM (worldDagInner ci) (imports w)

          -- Merge nodes and edges from parents
          let parent_nodes_merged = S.unions parent_nodes
          let parent_edges_merged = S.unions parent_edges

          -- Node produced by this world, if it's a new Island.
          -- Edges produced by this world, if it's a new Island. Note that any
          -- edges pointing to something in a cluster need to be redirected to
          -- the cluser node.
          let (my_nodes, my_edges) = case w_origin w of
                MergedWorlds _ -> (S.empty, S.empty)
                NewWorld _ wi  ->
                  -- ASSERT: wi_mod wi == mish ?
                  ( S.singleton (show (wi_mod wi), islandInstCount wi)
                  -- , S.fromList [ (show mish, n, show imp_m)
                  --                  | (imp_m, imp_w) <- imports w
                  --                  , n <- S.toList $ imp_nodes mish imp_m imp_w ] )
                  , S.unions [ S.map (mkE imp_m) (imp_nodes mish imp_m imp_w)
                                 | (imp_m, imp_w) <- imports w ]
                  )
                  where
                    mkE imp_m n = (show mish, n, show imp_m)

          return ( S.union my_nodes parent_nodes_merged
                 , S.union my_edges parent_edges_merged )


clusterNode :: Moduleish -> World -> Node
clusterNode mish w = (n, worldInstCount w)
  where
    n = "<" ++ mishPkgStr mish ++ ">"

-- | The canonical set of nodes corresponding to this World, i.e., the smallest
--   set of Islands whose total reachability set includes every Island in this
--   World.
worldCanonicalNodes :: World -> S.Set N
worldCanonicalNodes w = S.map (show . wi_mod) $ canonicalIslands w

worldAllNodes :: World -> S.Set N
worldAllNodes w =
  S.fromList [ show (wi_mod wi) | wi <- eltsUFM (w_wimap w) ]

--------------- OUTPUT -------------------

 
graphToDotPng :: FilePath -> DAG -> IO Bool
graphToDotPng fpre (nodes,edges) =
  handle (\(e::GraphvizException) -> return False) $
    addExtension (runGraphviz (graphElemsToDot dagParams nodes' edges')) Png fpre >> return True
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

dagParams :: GraphvizParams N NL EL () NL
dagParams = nonClusteredParams
      { isDirected = True
      , globalAttributes = [ GraphAttrs [RankDir FromBottom]
                           , EdgeAttrs [FontSize 8.0] ]
      , fmtEdge = \(n1, n2, el) -> [toLabel (mkEdgeLabel n1 n2 el)]
      }
  where
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
