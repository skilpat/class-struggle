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
import UniqSet

import ReadOrphans hiding (processPkg, processAllPkgs, processMod)
import World


data Ctx = Ctx { ctx_map  :: UniqFM (ModIface, World)
               , ctx_pkgs :: UniqSet PackageId }

type CtxM a = StateT Ctx Ghc a

instance Monoid Ctx where
  mempty = Ctx { ctx_map  = emptyUFM
               , ctx_pkgs = emptyUniqSet }

  mappend Ctx { ctx_map  = map1
              , ctx_pkgs = pkgs1 }
          Ctx { ctx_map  = map2
              , ctx_pkgs   = pkgs2 } =
    Ctx { ctx_map = plusUFM map1 map2
        , ctx_pkgs   = unionUniqSets pkgs1 pkgs2 }

instance Outputable Ctx where
  ppr ctx = text "ctx saw packages:" <+> ppr (ctx_pkgs ctx)
            $+$ vcat docs

    where
      docs = [ ppr mod <+> text "->" <+> int (worldInstCount w) <+> text "instances"
             | (mod, (iface,w)) <- ufmToList (ctx_map ctx) ]


buildCtx :: [String] -> IO Ctx
buildCtx pkgs = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  runGhc (Just libdir) $ do

    -- we have to call 'setSessionDynFlags' before doing
    -- everything else
    dflags <- getSessionDynFlags
    let dflags' = dopt_set dflags Opt_HideAllPackages
    setSessionDynFlags $ dflags' { ghcMode      = OneShot
                                 , hscTarget    = HscNothing
                                 --, thisPackage  = stringToPackageId "containers-0.5.0.0"
                                 -- , packageFlags = map ExposePackage pkgs
                                 }
    setTargets []
    -- setTargets =<< sequence [guessTarget "Test.hs" Nothing]
    load LoadAllTargets
    hscEnv <- getSession
    dflags <- getSessionDynFlags

    -- let pkgModMap = currentPkgModMap dflags requestedPkgs
    -- let mods = concat $ M.elems pkgModMap

    -- liftIO $ putStrLn $ showSDoc dflags $ ppr pkgModMap
    -- liftIO $ putStrLn ""

    -- forM_ mods $ \mod -> do
    --   liftIO $ putStrLn $ showSDoc dflags $ pprModule mod
    --   -- Found modLoc _ <- liftIO $ findModLocation hscEnv mod
    --   modLoc <- findModLocation mod
    --   let hiFile = ml_hi_file modLoc
    --   let tcInstances = do
    --         res <- readIface mod hiFile False
    --         return $ case res of
    --           Maybes.Succeeded modIface -> Just $ mi_insts modIface
    --           Maybes.Failed _ -> Nothing
    --   mbInsts <- liftIO $ initTcForLookup hscEnv tcInstances
    --   liftIO $ putStrLn $ case mbInsts of
    --     Just []    -> "  no instances"
    --     Just insts -> showSDoc dflags $ vcat $ map pprInstanceWithOrphan insts
    --     Nothing    -> "! lookup failed"
    --   liftIO $ putStrLn ""

    let pkg_mod_map = currentPkgModMap dflags pkgs
    ctx <- execStateT (processAllPkgs pkg_mod_map) mempty :: Ghc Ctx
    liftIO $ putStrLn $ showSDoc dflags $ ppr ctx
    return ctx


processAllPkgs :: PkgModMap -> CtxM ()
processAllPkgs pkg_mod_map = F.sequence_ $ M.mapWithKey processPkg pkg_mod_map


processPkg :: PackageId -> [Module] -> CtxM ()
processPkg pid mods = do
  ctx <- get
  dflags <- lift getSessionDynFlags

  -- Check whether already done.
  if ctxHasPkg ctx pid
    then return ()
    else do

      -- If not, process each of its mods.
      lift $ liftIO $ putStrLn $ showSDoc dflags $ text "*" <+> ppr pid
      mapM_ processIfMissing mods

      -- Record this package as done.
      updateCtxPkgs pid



processIfMissing :: Module -> CtxM ()
processIfMissing mod = do
  ctx <- get
  case lookupUFM (ctx_map ctx) mod of
    Just res -> return ()
    Nothing  -> processMod mod


lookupAndProcess :: Module -> CtxM (ModIface, World)
lookupAndProcess mod = do
  -- If we haven't processed this mod yet, do it now.
  processIfMissing mod
  -- Get the map and look it up.
  ctx <- get
  case lookupUFM (ctx_map ctx) mod of
    Just res -> return res
    Nothing  -> error $ "lookup failed for module " ++ moduleNameString (moduleName mod)


updateCtxMap :: Module -> ModIface -> World -> CtxM ()
updateCtxMap mod iface w = do
  ctx@Ctx{ctx_map = cm} <- get
  -- Update context's map with this Module and (ModIface, World).
  put $ ctx { ctx_map = addToUFM cm mod (iface, w) }


updateCtxPkgs :: PackageId -> CtxM ()
updateCtxPkgs pid = do
  ctx@Ctx{ctx_pkgs = pkgs} <- get
  -- Update context's pkg set with this one.
  put $ ctx { ctx_pkgs = addOneToUniqSet pkgs pid }


processMod :: Module -> CtxM ()
processMod mod = do
  hsc_env <- lift getSession
  dflags <- lift getSessionDynFlags
  lift $ liftIO $ putStrLn $ showSDoc dflags $ text "  -" <+> ppr (moduleName mod)

  -- Read the interface.
  iface  <- lift $ ifaceForMod mod

  -- Get the worlds of imports, after recursively process them first.
  let imps = modImports mod iface
  imp_worlds <- mapM (fmap snd . lookupAndProcess) imps

  -- Type check the interface so that the instances are in the right format.
  md <- lift $ liftIO $ initTcForLookup hsc_env $ typecheckIface iface
  let insts = md_insts md

  -- TODO: family instances!

  case newWorld imp_worlds mod insts of
    Just w  -> updateCtxMap mod iface w
    Nothing -> do
      lift $ liftIO $ putStrLn $ showSDoc dflags $ text "! error: failed to create world for " <+> ppr mod
      updateCtxMap mod iface emptyWorld


ctxHasPkg :: Ctx -> PackageId -> Bool
ctxHasPkg Ctx {ctx_pkgs = pids} pid = elementOfUniqSet pid pids

modImports :: Module -> ModIface -> [Module]
modImports this_mod iface = concatMap modFromUsage (mi_usages iface)
  where
    modFromUsage (UsagePackageModule {usg_mod = mod})    = [mod]
    modFromUsage (UsageHomeModule {usg_mod_name = name}) = [mkHomeMod name]
    modFromUsage _ = []
    mkHomeMod name = mkModule (modulePackageId this_mod) name