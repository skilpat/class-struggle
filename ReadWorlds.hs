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

import LoadIface
import Maybes

import Moduleish
import Util
import World

-- | A context that maps a Moduleish to its typechecked interface and world.
data Ctx = Ctx { ctx_map  :: UniqFM (Moduleish, ModIface, World)
               , ctx_pkgs :: UniqSet PackageId }

-- | A wrapper around the Ghc monad that keeps up with a Ctx as state.
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
  ppr ctx = text "ctx saw packages:" <+> ppr (uniqSetToList (ctx_pkgs ctx))
            $+$ pprCtxEntries ctx []


pprCtxEntries :: Ctx -> [String] -> SDoc
pprCtxEntries ctx mods = sep $ catMaybes maybeEntryDocs
  where
    maybeEntryDocs = [ pprEntry mish w | (mish, _, w) <- eltsUFM (ctx_map ctx) ]
    
    pprEntry mish w
      | shouldPrint mish = Just $ ppr mish <+> text "->" <+> pprIslands (w_wimap w)
      | otherwise        = Nothing

    shouldPrint mish
      | null mods                   = True
      | mishModStr mish `elem` mods = True
      | otherwise                   = False


-- | Given a list of module names and a Ctx, print out the World of any modules
--   whose names occur in the list.
printCtx :: [String] -> Ctx -> IO ()
printCtx mods ctx = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  runGhc (Just libdir) $ printSDoc $ pprCtxEntries ctx mods
        

buildCtx :: [String] -> IO Ctx
buildCtx pkgs = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  runGhc (Just libdir) $ do

    -- we have to call 'setSessionDynFlags' before doing
    -- everything else
    dflags <- getSessionDynFlags
    let dflags' = gopt_set dflags Opt_HideAllPackages
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
    --liftIO $ putStrLn $ showSDoc dflags $ ppr ctx
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

      -- If not, process each of its mods. Since we are processing this package
      -- from the outside, we are looking for *non-boot* modules to process,
      -- hence the `mkModuleish` call.
      lift $ liftIO $ putStrLn $ showSDoc dflags $ text "*" <+> ppr pid
      mapM_ (processIfMissing . mkModuleish) mods

      -- Record this package as done.
      updateCtxPkgs pid



processIfMissing :: Moduleish -> CtxM ()
processIfMissing mish = do
  ctx <- get
  case lookupUFM (ctx_map ctx) mish of
    Just res -> return ()
    Nothing  -> processMod mish


lookupAndProcess :: Moduleish -> CtxM (ModIface, World)
lookupAndProcess mish = do
  -- If we haven't processed this mish yet, do it now.
  processIfMissing mish
  -- Get the map and look it up.
  ctx <- get
  case lookupUFM (ctx_map ctx) mish of
    Just (_, iface, w) -> return (iface, w)
    Nothing  -> error $ "lookup failed for module " ++ (mishModStr mish)


processMod :: Moduleish -> CtxM ()
processMod mish = do
  let modsToPrint = [] -- ["Data.Type.Coercion"]
  let maybeDo m
        | mishModStr mish `elem` modsToPrint = m
        | otherwise                          = return ()

  -- Read the interface.
  iface  <- lift $ readIfaceForMish mish

  -- Get the worlds of imports, after recursively processing them first.
  let imp_mishes = importedMishes (mish_mod mish) iface
  imp_worlds <- mapM (fmap snd . lookupAndProcess) imp_mishes

  -- Type check the interface so that the instances are in the right format.
  insts <- lift $ readInstsFromIface iface

  -- Print module name and instances found.
  maybeDo $ do
    lift $ printSDoc $ text "  -" <+> ppr mish
    F.forM_ insts $ \inst ->
      lift $ printSDoc $ text "    -" <+> ppr inst

  -- TODO: family instances!

  case newWorld imp_worlds mish insts of
    Just w  -> do
      maybeDo $ do
        lift $ printSDoc $ text "new world's island ..."
        let island = fromJust $ lookupIsland w mish
        lift $ printSDoc $ ppr island
        F.forM_ (islandInsts island) $ \inst ->
          lift $ printSDoc $ text "    -" <+> ppr inst
      
      -- Store the iface and newly created world for this module.
      updateCtxMap mish iface w


    Nothing -> do
      lift $ printSDoc $ text "! error: failed to create world for " <+> ppr mish
      updateCtxMap mish iface emptyWorld


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
  ctx@Ctx{ctx_map = cm} <- get
  -- Update context's map with this Moduleish and (ModIface, World).
  put $ ctx { ctx_map = addToUFM cm mish (mish, iface, w) }


updateCtxPkgs :: PackageId -> CtxM ()
updateCtxPkgs pid = do
  ctx@Ctx{ctx_pkgs = pkgs} <- get
  -- Update context's pkg set with this one.
  put $ ctx { ctx_pkgs = addOneToUniqSet pkgs pid }


ctxHasPkg :: Ctx -> PackageId -> Bool
ctxHasPkg Ctx {ctx_pkgs = pids} pid = elementOfUniqSet pid pids