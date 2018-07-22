module ReadWorlds where

import Control.Monad.State hiding (liftIO)
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashSet as HS
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
  lift $ printSDoc $ text "Done processing selected packages!"

  -- Get all depended-upon packages.
  depended_pids <- get >>= return . ctx_pkgs

  -- Create a map for the unselected but depended-upon packages and modules
  let depended (pid, _) = S.member pid depended_pids
  let pkg_mod_map_depended = M.filter depended pkg_mod_map_unselected

  lift $ printSDoc $
    text "Processing unselected but depended-upon packages:" <+>
    hsep (map (ppr . fst) (M.elems pkg_mod_map_depended))
  F.sequence_ $ M.mapWithKey processPkg pkg_mod_map_depended
  lift $ printSDoc $ text "Done processing unselected packages!"

  return ()


processPkg :: String -> (PackageId, [Module]) -> CtxM ()
processPkg pname (pid, mods) = do
  -- Process each of its mods. Since we are processing this package
  -- from the outside, we are looking for *non-boot* modules to process,
  -- hence the `mkModuleish` call.
  lift $ printSDoc $ text "*" <+> ppr pid
  results <- mapM ((lookupOrProcess 0) . mkModuleish) mods
  lift $ printSDoc $ text " finished processing " <+> ppr pid

  -- Record this package's world
  pkg_world <- runCacheCtx $ checkMergeList [ w | (_,_,w,_) <- results ] Nothing
  case (w_consis pkg_world) of
    False -> lift $ printSDoc $ text "! warning: failed to merge pkg world for" <+> ppr pid
    True -> lift $ printSDoc $ text " okay world; islands: "
              <+> ppr (worldIslandCount pkg_world) <+> text "; insts: "
              <+> ppr (worldInstCount pkg_world)

  updateCtxPkg pid pkg_world $ S.unions [ c | (_,_,_,c) <- results]


lookupOrProcess :: Int -> Moduleish -> CtxM CtxEntry
lookupOrProcess depth mish = do
  ctx <- get
  entry <- case lookupUFM (ctx_map ctx) mish of
    Just r  -> do
      lift $ printSDoc $ brackets $ int depth <+> text ": " <+> ppr mish <+> text ": looked up"
      return r
    Nothing -> do
      lift $ printSDoc $ brackets $ int depth <+> text ": " <+> ppr mish <+> text ": processing new"
      processMod depth mish

  lift flush
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
  let (value, cons_cache') = runState cache_ctx_comp cons_cache

  -- Update the WorldCtx's cache with that result
  modify $! \ctx -> ctx { ctx_cache = cons_cache' }

  -- Return the value
  return value

-- processMod :: Int -> Moduleish -> CtxM CtxEntry
-- processMod depth mish = do

--   -- Print everything at recursive depth, and flush output buffer

--   let p sdoc = do
--         lift $ printSDoc $ text (replicate depth '>') <+> sdoc
--         lift flush

--   -- Print module name
--   p $ text "-" <+> ppr mish

--   let modsToPrint = ["Vectorise.Exp"]
--   let maybeDo m
--         | mishModStr mish `elem` modsToPrint = m
--         | otherwise                          = return ()

--   -- Read the interface.
--   p $ text " # reading module interface"
--   iface <- lift $ readIfaceForMish mish

--   -- Get the worlds of imports, after recursively processing them first.
--   let imp_mishes = importedMishes (mish_mod mish) iface
--   p $ text " # found" <+> int (length imp_mishes) <+> text "imports: " <+> ppr imp_mishes
--   p $ text " # processing imports"
--   imp_results <- mapM (lookupOrProcess $ depth + 1) imp_mishes
--   p $ text " # done processing imports"
--   let imports = [(m,w) | (m,_,w,_) <- imp_results ]

--   -- Type check the interface so that the instances are in the right format.
--   p $ text " # reading instances from interface"
--   local_insts <- lift $ readInstsFromIface iface
--   p $ text " # found" <+> int (length local_insts) <+> text "instances"

--   -- Print instances found.
--   maybeDo $ do
--     F.forM_ local_insts $ \inst ->
--       p $ text " -" <+> ppr inst

--   -- TODO: family instances!

--   -- Get consistency cache
--   cons_cache <- get >>= return . ctx_cache

--   --when (isJust mb_bad_island) $ do
--   --  p $ text "! warning: created inconsistent world for" <+> ppr mish

--     -- Just w  -> do
--     --   -- maybeDo $ do
--     --   --   p $ text "new world's island ..."
--     --   --   let island = fromJust $ lookupIsland w mish
--     --   --   p $ ppr island
--     --   --   F.forM_ (islandInsts island) $ \inst ->
--     --   --     p $ text "    -" <+> pprInstanceHdr inst

--   -- Are any imported worlds inconsistent?
--   let imp_inconss = S.unions [ c | (_,_,_,c) <- imp_results]
--   --unless (S.null imp_inconss) $ do
--   --  p $ text "! warning: inheriting inconsistency from:"
--   --                     <+> hsep (map ppr (S.toAscList imp_inconss))

--   n <- getNumProcessed
--   when (n >= 690) $ do
--     p $ text " # new world:" <+> pprIslands (w_wimap w)
--     p $ text " # maybe bad island:" <+> ppr mb_bad_island

--   -- Check for inconsistency among imports
--   p $ text " # checking inconsistency among " <+> ppr (length imports) <+> text " imports"
--   mb_bad_islands <- runCacheCtx $! checkMergeableListBlame (map snd imports)

--   -- Check for locals consistency
--   p $ text " # checking inconsistency among " <+> ppr (length local_insts) <+> text " locals"
--   let bad_locals = localInconsistentInstances local_insts

--   -- Try to create a new world.
--   p $ text " # newWorldFromImports... "
--   (w, mb_bad_island) <- runCacheCtx $! newWorldFromImports imports mish local_insts

--   unless (null bad_locals) $ do -- inconsistency among locals
--     p $ text "! warning: creating inconsistency @ locals:"
--     -- p $ text "    inconsistent pairs:" <+> ppr bad_locals
--     forM_ bad_locals $ \bad_pair -> do
--       p $ text "    * between:" <+> ppr (fst bad_pair)
--       p $ text "          and:" <+> ppr (snd bad_pair)

--   when (isJust mb_bad_islands) $ do -- inconsistency among imports
--     p $ text "! warning: creating inconsistency @ imports:"
--     p $ text "    island 1:" <+> ppr (fst (fromJust mb_bad_islands))
--     p $ text "    island 2:" <+> ppr (snd (fromJust mb_bad_islands))
--     p $ text "    island 1 instances:" <+> ppr (islandInsts (fst (fromJust mb_bad_islands)))
--     p $ text "    island 2 instances:" <+> ppr (islandInsts (snd (fromJust mb_bad_islands)))

--   when (isJust mb_bad_island) $ do -- inconsistency btw imports & locals
--     p $ text "! warning: creating inconsistency @ imports+locals:"
--     p $ text "    parent island:" <+> ppr (fromJust mb_bad_island)
--     p $ text "    parent island instances:" <+> ppr (islandInsts (fromJust mb_bad_island))
--     p $ text "    local instances:" <+> sep (map ppr local_insts)

--   -- Record all inherited and created inconsistencies
--   let inconss
--         | isJust mb_bad_islands || isJust mb_bad_island || not (null bad_locals) = S.insert mish imp_inconss
--         | otherwise = imp_inconss
      
--   -- Store the iface and newly created world for this module.
--   p $ text " # updating context"
--   updateCtxMap mish iface w inconss
--   p $ text " # done with module " <+> ppr mish

--   num <- getNumProcessed
--   cache_size <- getCacheSize
--   p $ text " # processed:" <+> int num
--   p $ text " # cache size:" <+> int cache_size

--   return (mish, iface, w, inconss)


processMod :: Int -> Moduleish -> CtxM CtxEntry
processMod depth mish = do

  -- Print everything at recursive depth, and flush output buffer

  let p sdoc = do
        lift $ printSDoc $ text (replicate depth '>') <+> sdoc
        lift flush

  -- Print module name
  p $ text "-" <+> ppr mish

  let modsToPrint = ["Vectorise.Exp"]
  let maybeDo m
        | mishModStr mish `elem` modsToPrint = m
        | otherwise                          = return ()

  -- Read the interface.
  p $ text " # reading module interface"
  iface <- lift $! readIfaceForMish mish

  -- Get the names and worlds of imports, after recursively processing them first.
  let imp_mishes = importedMishes (mish_mod mish) iface
  p $ text " # found" <+> int (length imp_mishes) <+> text "imports: " <+> ppr imp_mishes
  imp_results <- mapM (lookupOrProcess $ depth + 1) imp_mishes
  let imports = [(m,w) | (m,_,w,_) <- imp_results ]
  p $ text " # done processing" <+> int (length imp_mishes) <+> text "imports"

  -- Type check the interface so that the instances are in the right format.
  p $ text " # reading instances from interface"
  local_insts <- lift $! readInstsFromIface iface
  p $ text " # found" <+> int (length local_insts) <+> text "instances"

  -- Print instances found, if this mod was selected to print
  maybeDo $ do
    F.forM_ local_insts $ \inst ->
      p $ text " -" <+> ppr inst

  num <- getNumProcessed
  cache_size <- getCacheSize
  p $ text " # processed:" <+> int num
  p $ text " # cache size:" <+> int cache_size

  -- Are any imported worlds inconsistent?
  let imp_inconss = S.unions [ c | (_,_,_,c) <- imp_results]
  unless (S.null imp_inconss) $ do
   p $ text "! warning: inheriting inconsistency from:"
                      <+> hsep (map ppr (S.toAscList imp_inconss))

  -- Check for locals consistency
  p $ text " # checking inconsistency among" <+> int (length local_insts) <+> text "locals"
  let mb_bad_locals = localInconsistentInstancesBlame $! local_insts

  -- Try to create a new world; its origin will contain newly created inconsistencies
  p $ text " # checking and creating new world"
  w_new <- runCacheCtx $!
      checkNewWorldFromImports imports mish (local_insts, mb_bad_locals)

  -- Print each of the new inconsistencies that were found (including locals)
  printWorldNewInconsistencies p w_new

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


printWorldNewInconsistencies :: (SDoc -> CtxM ()) -> World -> CtxM ()
printWorldNewInconsistencies p w = case w_origin w of
    MergedWorlds _ _ Nothing -> return ()
    MergedWorlds _ _ (Just clashes) -> printImpClashes clashes
    NewWorld _ _ nw_inconss -> forM_ nw_inconss printNWI
  where
    printImpClashes clashes = do
      p $ text "! warning: creating inconsistency @ imports:"
      forM_ clashes $ \(mish, inst) ->
        p $ text " -" <+> ppr mish <+> ppr inst

    printNWI (AmongImports clashes) = printImpClashes clashes
    printNWI (AmongLocals inst_clashes) = do
        p $ text "! warning: creating inconsistency @ locals:"
        forM_ inst_clashes $ \(inst1, inst2) -> do
          p $ text "    - between:" <+> ppr inst1
          p $ text "          and:" <+> ppr inst2
    printNWI (BetweenImportsAndLocals clashes) = do
        p $ text "! warning: creating inconsistency @ imports+locals:"
        forM_ clashes $ \(mish, inst) ->
          p $ text " -" <+> ppr mish <+> ppr inst


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
    mishFromUsage (UsagePackageModule {usg_mod = mod})    = [mkModuleish mod]
    mishFromUsage (UsageHomeModule {usg_mod_name = name}) = [mkHomeMish name]
    mishFromUsage _ = []

    -- Make a Moduleish for a home package module. Check the ModIface's
    -- Dependencies list to determine whether this is a boot file or not.
    mkHomeMish name = Moduleish (mkModule (modulePackageId this_mod) name)
                                (fromJust $ lookup name (dep_mods (mi_deps iface)))




