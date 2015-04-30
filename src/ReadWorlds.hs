module ReadWorlds where

import Control.Monad.State hiding (liftIO)
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.Maybe
  ( mapMaybe )

import DynFlags
import GHC
import GHC.Paths
-- import GhcMonad(liftIO)
import HscTypes
import Module
import Outputable
import TcIface
import TcRnMonad
import UniqFM
--import UniqSet

import LoadIface
import Maybes

import Moduleish
import ReadUtils
import World
import WorldCtx


        

-- | Build a Ctx mapping modules to worlds and interfaces, given an absolute
--   path to a cabal sandbox conf and a list of package names (minus versions)
--   to check.
buildCtx :: String -> [String] -> IO Ctx
buildCtx sandbox_path req_pkgs = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  runGhc (Just libdir) $ do

    -- Try to use a sandbox pkg db, if one is passed in.
    updateDynFlags [ "-hide-all-packages"
                   , "-no-user-package-db"
                   , "-package-db " ++ sandbox_path ]
    -- unless (null sandbox_path) $
    --        updateDynFlags [ "-no-user-package-db"
    --                       , "-package-db " ++ sandbox_path ]

    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { ghcMode       = OneShot
                                , hscTarget     = HscNothing
                                }
    setTargets []
    -- setTargets =<< sequence [guessTarget "Test.hs" Nothing]
    load LoadAllTargets

    -- Start doing stuff.
    pkg_mod_maps <- currentPkgModMap req_pkgs

    -- liftIO $ putStrLn "LOADED PACKAGES:"
    -- printPkgs
    --let pkg_names = M.keys pkg_mod_map

    ctx <- execStateT (processAllPkgs pkg_mod_maps) mempty :: Ghc Ctx
    --liftIO $ putStrLn $ showSDoc dflags $ ppr ctx
    --printSDoc $ pprCtxEntries ctx ["base:Prelude"]

    return ctx


processAllPkgs :: (PkgModMap, PkgModMap) -> CtxM ()
processAllPkgs (pkg_mod_map_selected, pkg_mod_map_unselected) = do
  -- Process each of the selected packages.
  lift $ printSDoc $
    text "Processing selected packages:" <+>
    hsep (map (ppr . fst) (M.elems pkg_mod_map_selected))
  F.sequence_ $ M.mapWithKey processPkg pkg_mod_map_selected

  -- Get all depended-upon packages.
  depended_pids <- get >>= return . ctx_pkgs

  -- Create a map for the unselected but depended-upon packages and modules
  let depended (pid, _) = S.member pid depended_pids
  let pkg_mod_map_depended = M.filter depended pkg_mod_map_unselected

  lift $ printSDoc $
    text "Processing unselected but depended-upon packages:" <+>
    hsep (map (ppr . fst) (M.elems pkg_mod_map_depended))
  F.sequence_ $ M.mapWithKey processPkg pkg_mod_map_depended

  return ()


processPkg :: String -> (PackageId, [Module]) -> CtxM ()
processPkg pname (pid, mods) = do
  -- Process each of its mods. Since we are processing this package
  -- from the outside, we are looking for *non-boot* modules to process,
  -- hence the `mkModuleish` call.
  lift $ printSDoc $ text "*" <+> ppr pid
  results <- mapM (lookupOrProcess . mkModuleish) mods

  -- Record this package's world
  let pkg_world = mergeList [ w | (_,_,w,_) <- results ]
  -- unless (w_consis pkg_world) $ do
  --   lift $ printSDoc $
  --     text "! warning: failed to merge pkg world for" <+> ppr pid

  updateCtxPkg pid pkg_world $ S.unions [ c | (_,_,_,c) <- results]


lookupOrProcess :: Moduleish -> CtxM CtxEntry
lookupOrProcess mish = do
  ctx <- get
  case lookupUFM (ctx_map ctx) mish of
    Just r  -> return r
    Nothing -> processMod mish



-- mkConsistency :: Moduleish -> [(Moduleish, World)] -> CtxM Consistency
-- mkConsistency mish imps = do
--   -- gather up consistency from imports
--   inconsistent_mishes <- liftM concat $ mapM ()

--   -- add self if imported worlds don't merge


processMod :: Moduleish -> CtxM CtxEntry
processMod mish = do
  let modsToPrint = ["Prelude","Test1.Left","Test1.Right"]
  let maybeDo m
        | mishModStr mish `elem` modsToPrint = m
        | otherwise                          = return ()

  -- Read the interface.
  iface  <- lift $ readIfaceForMish mish

  -- Get the worlds of imports, after recursively processing them first.
  let imp_mishes = importedMishes (mish_mod mish) iface
  imp_results <- mapM lookupOrProcess imp_mishes
  let imports = [(m,w) | (m,_,w,_) <- imp_results ]

  -- Are any imported worlds inconsistent?
  let imp_inconss = S.unions [ c | (_,_,_,c) <- imp_results]
  let inconss
        | not $ mergeableList (map snd imports) = S.insert mish imp_inconss
        | otherwise = imp_inconss

  -- Type check the interface so that the instances are in the right format.
  insts <- lift $ readInstsFromIface iface

  -- Print module name and instances found.
  lift $ printSDoc $ text "  -" <+> ppr mish
  maybeDo $ do
    F.forM_ insts $ \inst ->
      lift $ printSDoc $ text "    -" <+> ppr inst

  -- TODO: family instances!

  -- Try to create a new world.
  let w = newWorldFromImports imports mish insts
  -- unless (w_consis w) $ do
  --   lift $ printSDoc $ text "! warning: created inconsistent world for " <+> ppr mish

    -- Just w  -> do
    --   -- maybeDo $ do
    --   --   lift $ printSDoc $ text "new world's island ..."
    --   --   let island = fromJust $ lookupIsland w mish
    --   --   lift $ printSDoc $ ppr island
    --   --   F.forM_ (islandInsts island) $ \inst ->
    --   --     lift $ printSDoc $ text "    -" <+> pprInstanceHdr inst
      
  -- Store the iface and newly created world for this module.
  updateCtxMap mish iface w inconss
  return (mish, iface, w, inconss)



importedMishes :: Module -> ModIface -> [Moduleish]
importedMishes this_mod iface = concatMap mishFromUsage (mi_usages iface)
  where
    mishFromUsage (UsagePackageModule {usg_mod = mod})    = [mkModuleish mod]
    mishFromUsage (UsageHomeModule {usg_mod_name = name}) = [mkHomeMish name]
    mishFromUsage _ = []

    -- Make a Moduleish for a home package module. Check the ModIface's
    -- Dependencies list to determine whether this is a boot file or not.
    mkHomeMish name = Moduleish (mkModule (modulePackageId this_mod) name)
                                (fromJust $ lookup name (dep_mods (mi_deps iface)))




