module ReadWorlds where

import Control.Monad.State hiding (liftIO)
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F

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



-- | A wrapper around the Ghc monad that keeps up with a Ctx as state.
type CtxM a = StateT Ctx Ghc a
        

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
    pkg_mod_map <- currentPkgModMap req_pkgs

    -- liftIO $ putStrLn "LOADED PACKAGES:"
    -- printPkgs
    --let pkg_names = M.keys pkg_mod_map

    ctx <- execStateT (processAllPkgs pkg_mod_map) mempty :: Ghc Ctx
    --liftIO $ putStrLn $ showSDoc dflags $ ppr ctx
    --printSDoc $ pprCtxEntries ctx ["base:Prelude"]

    return ctx


processAllPkgs :: PkgModMap -> CtxM ()
processAllPkgs pkg_mod_map = F.sequence_ $ M.mapWithKey processPkg pkg_mod_map


processPkg :: String -> (PackageId, [Module]) -> CtxM ()
processPkg pname (pid, mods) = do
  -- Process each of its mods. Since we are processing this package
  -- from the outside, we are looking for *non-boot* modules to process,
  -- hence the `mkModuleish` call.
  lift $ printSDoc $ text "*" <+> ppr pid
  mws <- mapM (lookupOrProcess . mkModuleish) mods

  -- Record this package's world
  pkg_world <- case mergeList (map snd mws) of
    Just w -> return w
    Nothing -> do
      lift $ printSDoc $
        text "! warning: failed to merge pkg world for" <+> ppr pid
      return emptyWorld

  updateCtxPkg pid pkg_world


lookupOrProcess :: Moduleish -> CtxM (ModIface, World)
lookupOrProcess mish = do
  ctx <- get
  case lookupUFM (ctx_map ctx) mish of
    Just (_, iface, w) -> return (iface, w)
    Nothing            -> processMod mish


processMod :: Moduleish -> CtxM (ModIface, World)
processMod mish = do
  let modsToPrint = ["Prelude","Test1.Left","Test1.Right"]
  let maybeDo m
        | mishModStr mish `elem` modsToPrint = m
        | otherwise                          = return ()

  -- Read the interface.
  iface  <- lift $ readIfaceForMish mish

  -- Get the worlds of imports, after recursively processing them first.
  let imp_mishes = importedMishes (mish_mod mish) iface
  imp_worlds <- mapM (fmap snd . lookupOrProcess) imp_mishes

  -- Type check the interface so that the instances are in the right format.
  insts <- lift $ readInstsFromIface iface

  -- Print module name and instances found.
  lift $ printSDoc $ text "  -" <+> ppr mish
  maybeDo $ do
    F.forM_ insts $ \inst ->
      lift $ printSDoc $ text "    -" <+> ppr inst

  -- TODO: family instances!

  -- Try to create a new world.
  case newWorldFromImports (zip imp_mishes imp_worlds) mish insts of
    Just w  -> do
      -- maybeDo $ do
      --   lift $ printSDoc $ text "new world's island ..."
      --   let island = fromJust $ lookupIsland w mish
      --   lift $ printSDoc $ ppr island
      --   F.forM_ (islandInsts island) $ \inst ->
      --     lift $ printSDoc $ text "    -" <+> pprInstanceHdr inst
      
      -- Store the iface and newly created world for this module.
      updateCtxMap mish iface w
      return (iface, w)


    Nothing -> do
      lift $ printSDoc $ text "! error: failed to create world for " <+> ppr mish
      updateCtxMap mish iface emptyWorld
      return (iface, emptyWorld)


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

--------- CTX OPS --------------

updateCtxMap :: Moduleish -> ModIface -> World -> CtxM ()
updateCtxMap mish iface w = do
  ctx@Ctx{ctx_map = cmap} <- get
  -- Update context's map with this Moduleish and (ModIface, World).
  put $ ctx { ctx_map = addToUFM cmap mish (mish, iface, w) }


updateCtxPkg :: PackageId -> World -> CtxM ()
updateCtxPkg pid w = do
 ctx@Ctx{ctx_pkg_map = pmap} <- get
 -- Update context's pkg world map with this one.
 put $ ctx { ctx_pkg_map = addToUFM pmap pid (pid, w) }


