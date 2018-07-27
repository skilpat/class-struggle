module ReadWorlds where

import Control.Monad.State hiding (liftIO)
import Data.Monoid hiding ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.Foldable as F
import Data.Maybe
  ( mapMaybe )
import Data.List
  ( partition )

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
    text "== Processing selected packages" <+>
    hsep (map (ppr . fst) (M.elems pkg_mod_map_selected))
  F.sequence_ $ M.mapWithKey processPkg pkg_mod_map_selected
  lift $ printSDoc $ text "== Done processing selected packages"

  -- Get all depended-upon packages.
  depended_pids <- get >>= return . ctx_pkgs

  -- Create a map for the unselected but depended-upon packages and modules
  let depended (pid, _) = S.member pid depended_pids
  let pkg_mod_map_depended = M.filter depended pkg_mod_map_unselected

  lift $ printSDoc $
    text "== Processing unselected (but depended-upon) packages:" <+>
    hsep (map (ppr . fst) (M.elems pkg_mod_map_depended))
  F.sequence_ $ M.mapWithKey processPkg pkg_mod_map_depended
  lift $ printSDoc $ text "== Done processing unselected (but depended-upon) packages"

  return ()


processPkg :: String -> (PackageId, [Module]) -> CtxM ()
processPkg pname (pid, mods) = do
  -- Process each of its mods. Since we are processing this package
  -- from the outside, we are looking for *non-boot* modules to process,
  -- hence the `mkModuleishImpl` call.
  lift $ printSDoc $ text "*" <+> ppr pid
  results <- mapM ((lookupOrProcess 0) . mkModuleishImpl) mods

  -- Record this package's world
  pkg_world <- runCacheCtx $ checkMergeList [ w | (_,_,w,_) <- results ] Nothing
  let consis_str = if w_consis pkg_world then "consistent" else "inconsistent"
  lift $ printSDoc $ text "* finished package" <+> ppr pid <> colon
    <+> text "world:" <+> text consis_str
    <> text "; islands:" <+> int (worldIslandCount pkg_world)
    <> text "; instances:" <+> int (worldInstCount pkg_world)

  updateCtxPkg pid pkg_world $ S.unions [ c | (_,_,_,c) <- results]


lookupOrProcess :: Int -> Moduleish -> CtxM CtxEntry
lookupOrProcess depth mish = do
  ctx <- get
  entry <- case lookupUFM (ctx_map ctx) mish of
    Just r  -> do
      -- lift $ printSDoc $ brackets $ int depth <+> text ": " <+> ppr mish <+> text ": looked up"
      return r
    Nothing -> do
      -- lift $ printSDoc $ brackets $ int depth <+> text ": " <+> ppr mish <+> text ": processing new"
      processMod depth mish

  -- lift flush
  return $! entry



-- mkConsistency :: Moduleish -> [(Moduleish, World)] -> CtxM Consistency
-- mkConsistency mish imps = do
--   -- gather up consistency from imports
--   inconsistent_mishes <- liftM concat $ mapM ()

--   -- add self if imported worlds don't merge

-- Given an action in the WorldConsCtx monad, pass it the current cache, run
-- it, and update the cache.
runCacheCtx :: WorldConsCtx a -> CtxM a
runCacheCtx cache_ctx_comp = do
  -- Get the WorldConsCache
  cons_cache <- get >>= return . ctx_cache

  -- Run the computation using that cache and get a value
  (value, cons_cache') <- lift $ runStateT cache_ctx_comp cons_cache

  -- Update the WorldCtx's cache with that result
  modify $! \ctx -> ctx { ctx_cache = cons_cache' }

  -- Return the value
  return value


processMod :: Int -> Moduleish -> CtxM CtxEntry
processMod depth mish = do

  -- Print everything at recursive depth, and flush output buffer
  let p sdoc = do
        -- lift $ printSDoc $ text (replicate depth '>') <+> sdoc
        lift $ printSDoc $ space <+> sdoc
        lift flush

  -- let debug sdoc = p $ text "#" <+> sdoc
  let debug sdoc = return ()
  let warn sdoc = p $ text "! warning:" <+> ppr mish <> text ":" <+> sdoc


  -- Print module name
  p $ text "-" <+> ppr mish

  let modsToPrint = [] --["CmmNode", "Vectorise.Exp"]
  let maybeDo m
        | mishModStr mish `elem` modsToPrint = m
        | otherwise                          = return ()

  -- Read the interface.
  debug $ text "reading module interface"
  iface <- lift $! readIfaceForMish mish

  -- Get the names and worlds of imports, after recursively processing them first.
  let imp_mishes = importedMishes (mish_mod mish) iface
  debug $ text "found" <+> int (length imp_mishes) <+> text "imports: " <+> ppr imp_mishes
  imp_results <- mapM (lookupOrProcess $ depth + 1) imp_mishes
  let imports = [(m,w) | (m,_,w,_) <- imp_results ]
  debug $ text "done processing" <+> int (length imp_mishes) <+> text "imports"

  -- Type check the interface so that the instances are in the right format.
  debug $ text "reading instances from interface"
  local_insts <- lift $! readInstsFromIface iface
  debug $ text "found" <+> int (length local_insts) <+> text "instances"

  -- Print instances found, if this mod was selected to print
  maybeDo $ do
    F.forM_ local_insts $ \inst ->
      p $ text " -" <+> ppr inst

  num <- getNumProcessed
  cache_size <- getCacheSize
  debug $ text "processed:" <+> int num
  debug $ text "cache size:" <+> int cache_size

  -- Are any imported worlds inconsistent?
  let imp_inconss = S.unions [ c | (_,_,_,c) <- imp_results]
  unless (S.null imp_inconss) $ do
    warn $ text "inheriting inconsistency from:"
                      <+> (braces $ hcat $ punctuate (text ", ") $ map ppr (S.toAscList imp_inconss))

  -- Check for locals consistency
  debug $ text "checking inconsistency among" <+> int (length local_insts) <+> text "locals"
  let mb_bad_locals = localInconsistentInstancesBlame $! local_insts

  -- Try to create a new world; its origin will contain newly created inconsistencies
  debug $ text "checking and creating new world"
  w_new <- runCacheCtx $!
      checkNewWorldFromImports imports mish (local_insts, mb_bad_locals)

  -- Print each of the new inconsistencies that were found (including locals)
  printWorldNewInconsistencies p mish w_new

  -- n <- getNumProcessed
  -- when (n >= 690) $ do
  --   p $ text " # new world:" <+> pprIslands (w_wimap w)
  --   p $ text " # maybe bad island:" <+> ppr mb_bad_island

  -- Log all inherited and created inconsistencies
  let inconss
        -- if world created new inconsistencies, add this module to set
        | worldCreatesInconsistency w_new = S.insert mish imp_inconss
        -- otherwise reuse those from imports
        | otherwise = imp_inconss
      
  -- Store the iface and newly created world for this module.
  updateCtxMap mish () w_new inconss
  return $! (mish, (), w_new, inconss)


printWorldNewInconsistencies :: (SDoc -> CtxM ()) -> Moduleish -> World -> CtxM ()
printWorldNewInconsistencies p mish w = case w_origin w of
    MergedWorlds _ _ Nothing -> return ()
    MergedWorlds _ _ (Just clashes) -> printImpClashes clashes
    NewWorld _ _ nw_inconss -> forM_ nw_inconss printNWI
  where
    warn sdoc = p $ text "! warning:" <+> ppr mish <> text ":" <+> sdoc

    printImpClashes clashes = do
      warn $ text "creating inconsistency @ imports:"
      forM_ clashes $ \(mish, inst) ->
        p $ text " -" <+> ppr mish <+> ppr inst

    printNWI (AmongImports clashes) = printImpClashes clashes
    printNWI (AmongLocals inst_clashes) = do
        warn $ text "creating inconsistency @ locals:"
        forM_ inst_clashes $ \(inst1, inst2) -> do
          p $ text "    - between:" <+> ppr inst1
          p $ text "          and:" <+> ppr inst2
    printNWI (BetweenImportsAndLocals clashes) = do
        warn $ text "creating inconsistency @ imports+locals:"
        -- Separate the clashes for this local mod and for the imports.
        let (loc_clashes, imps_clashes) = partition ((== mish) . fst) clashes
        p $ text " - local instance(s):" <+> ppr (map snd loc_clashes)
        forM_ imps_clashes $ \(imp_mish, imp_inst) ->
          p $ text " - imported" <+> ppr imp_mish <+> text "instance:" <+> ppr imp_inst


getNumProcessed :: CtxM Int
getNumProcessed = do
  ctx <- get
  return $ sizeUFM $ ctx_map ctx


getCacheSize :: CtxM Int
getCacheSize = do
  ctx <- get
  return $ HS.size $ ctx_cache ctx


importedMishes :: Module -> ModIface -> [Moduleish]
importedMishes this_mod iface = concatMap mishFromUsage (mi_usages iface)
  where
    mishFromUsage (UsagePackageModule {usg_mod = mod})    = [mkModuleishImpl mod]
    mishFromUsage (UsageHomeModule {usg_mod_name = name}) = [mkHomeMish name]
    mishFromUsage _ = []

    -- Make a Moduleish for a home package module. Check the ModIface's
    -- Dependencies list to determine whether this is a boot file or not.
    mkHomeMish name = mkModuleish (mkModule (modulePackageId this_mod) name)
                                  (fromJust $! lookup name (dep_mods (mi_deps iface)))




