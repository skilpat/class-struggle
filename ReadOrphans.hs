module ReadOrphans where

--import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Traversable as T
-- import Distribution.Package(PackageName(..))
import DynFlags
import Finder
import GHC
import GHC.Paths
-- import GhcMonad(liftIO)
import HscTypes
import IfaceSyn
import LoadIface
import Maybes
import Module
import Outputable
import PackageConfig
import Packages
import System.Directory
import System.FilePath
import TcRnMonad
import Text.Printf
import UniqFM
-- import Unsafe.Coerce


stats :: [String] -> IO TotalOrphStats
stats req_pkgs = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  runGhc (Just libdir) $ do

    -- we have to call 'setSessionDynFlags' before doing
    -- everything else
    dflags <- getSessionDynFlags
    let dflags' = dopt_set dflags Opt_HideAllPackages
    setSessionDynFlags $ dflags' { ghcMode      = OneShot
                                 , hscTarget    = HscNothing
                                 --, thisPackage  = stringToPackageId "containers-0.5.0.0"
                                 -- , packageFlags = map ExposePackage req_pkgs
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

    let pkg_mod_map = currentPkgModMap dflags req_pkgs
    processAllPkgs pkg_mod_map


processAllPkgs :: PkgModMap -> Ghc TotalOrphStats
processAllPkgs pkg_mod_map = do
  dflags <- getSessionDynFlags

  pkg_orph_stats_map <- T.sequence $ M.mapWithKey processPkg pkg_mod_map

  liftIO $ putStrLn "========= TOTALS ========="
  liftIO $ putStrLn ""
  let calcTotalStats tos pos = do
        liftIO $ putStrLn $ showSDoc dflags $ ppr pos
        liftIO $ putStrLn ""
        return $ updateTotalOrphStats tos pos
        -- liftIO $ putStrLn $ showSDoc dflags $ ppr pid
        -- liftIO $ putStrLn $ "  orphan modules:   " ++ show num_orphs
        -- liftIO $ putStrLn $ "  orphan instances: " ++ show num_orphs

  tos_final <- F.foldlM calcTotalStats initTotalOrphStats pkg_orph_stats_map
  liftIO $ putStrLn $ showSDoc dflags $ ppr tos_final
  return tos_final
  -- liftIO $ putStrLn $ "TOTAL"
  -- liftIO $ putStrLn $ "  orphans:     " ++ show num_orphs
  -- liftIO $ putStrLn $ "  non-orphans: " ++ show num_non_orphs    


data TotalOrphStats = TotalOrphStats { tos_num_orphs      :: Int
                                     , tos_num_norphs     :: Int
                                     , tos_num_orph_mods  :: Int
                                     , tos_num_norph_mods :: Int
                                     , tos_num_no_inst    :: Int
                                     }

data PkgOrphStats = PkgOrphStats { pos_pid            :: PackageId
                                 , pos_num_orphs      :: Int
                                 , pos_num_norphs     :: Int
                                 , pos_num_orph_mods  :: Int
                                 , pos_num_norph_mods :: Int
                                 , pos_num_no_inst    :: Int
                                 }

data ModOrphStats = ModOrphStats { mos_name       :: ModuleName
                                 , mos_pid        :: PackageId
                                 , mos_num_orphs  :: Int
                                 , mos_num_norphs :: Int
                                 }


instance Outputable TotalOrphStats where
  ppr (TotalOrphStats noi nni nom nnm nnoinst) =
    text "TOTAL"
    $+$ pprStats "orphan modules" nom nnm
    $+$ pprStats "orphan instances" noi nni
    $+$ pprStats "mods w/o instances" nnoinst ((nom+nnm)-nnoinst)

instance Outputable PkgOrphStats where
  ppr (PkgOrphStats pid noi nni nom nnm nnoinst) =
    ppr pid
    $+$ pprStats "orphan modules" nom nnm
    $+$ pprStats "orphan instances" noi nni
    $+$ pprStats "mods w/o instances" nnoinst ((nom+nnm)-nnoinst)

instance Outputable ModOrphStats where
  ppr (ModOrphStats mname pid noi nni) =
    ppr mname
    $+$ pprStats "orphan instances" noi nni

pprStats :: String -> Int -> Int -> SDoc
pprStats thing n1 n2 = text (printf "  %-18s: %3d of %3d" thing n1 (n1+n2))

initTotalOrphStats :: TotalOrphStats
initTotalOrphStats = TotalOrphStats 0 0 0 0 0

initPkgOrphStats :: PackageId -> PkgOrphStats
initPkgOrphStats pid = PkgOrphStats pid 0 0 0 0 0

-- mergeTotalOrphStats :: TotalOrphStats -> TotalOrphStats -> TotalOrphStats
-- mergeTotalOrphStats (TotalOrphStats noi1 nni1 nom1 nnm1) (TotalOrphStats noi2 nni2 nom2 nnm2) =
--   TotalOrphStats (noi1+noi2) (nni1+nni2) (nom1+nom2) (nnm1+nnm2)

-- mergePkgOrphStats :: PkgOrphStats -> PkgOrphStats -> PkgOrphStats
-- mergePkgOrphStats (PkgOrphStats pid1 noi1 nni1 nom1 nnm1) (PkgOrphStats pid2 noi2 nni2 nom2 nnm2) =
--   if pid1 /= pid2
--     then error "cannot merge distinct PkgOrphStats"
--     else PkgOrphStats pid1 (noi1+noi2) (nni1+nni2) (nom1+nom2) (nnm1+nnm2)

-- mergeModOrphStats :: ModOrphStats -> ModOrphStats -> ModOrphStats
-- mergeModOrphStats (ModOrphStats name1 pid1 orphs1 norphs1) (ModOrphStats name2 pid2 orphs2 norphs2) =
--   if name1 /= name2 || pid1 /= pid2
--     then error "cannot merge distinct ModOrphStats"
--     else ModOrphStats name1 pid1 (orphs1+orphs2) (norphs1+norphs2)

mosIsOrphan :: ModOrphStats -> Bool
mosIsOrphan mos = mos_num_orphs mos > 0

updatePkgOrphStats :: PkgOrphStats -> ModOrphStats -> PkgOrphStats
updatePkgOrphStats (PkgOrphStats pid noi nni nom nnm nnoinst) mos@(ModOrphStats m pid' orphs norphs) =
  if pid /= pid'
    then error "cannot update PkgOrphStats with non-matching ModOrphStats"
    else PkgOrphStats pid noi' nni' nom' nnm' nnoinst'
      where
        noi' = noi+orphs
        nni' = nni+norphs
        nom' | mosIsOrphan mos = nom+1
             | otherwise       = nom
        nnm' | mosIsOrphan mos = nnm
             | otherwise       = nnm+1
        nnoinst' | orphs == 0 && norphs == 0 = nnoinst
                 | otherwise                 = nnoinst+1

updateTotalOrphStats :: TotalOrphStats -> PkgOrphStats -> TotalOrphStats
updateTotalOrphStats (TotalOrphStats noi1 nni1 nom1 nnm1 nnoinst1)
                 pos@(PkgOrphStats pid noi2 nni2 nom2 nnm2 nnoinst2) =
  TotalOrphStats (noi1+noi2) (nni1+nni2) (nom1+nom2) (nnm1+nnm2) (nnoinst1+nnoinst2)

processPkg :: PackageId -> [Module] -> Ghc PkgOrphStats
processPkg pid mods = do
  dflags <- getSessionDynFlags

  -- print package id
  liftIO $ putStrLn "*****************************"
  liftIO $ putStrLn $ showSDoc dflags $ ppr pid
  liftIO $ putStrLn "*****************************"

  -- process each mod
  let f pos mod = do
          -- print module name
          printSDoc $ ppr $ moduleName mod
          -- process module contents, printing any orphans
          mos <- processMod mod
          -- print stats for module
          printSDoc $ pprStats "= orphans" (mos_num_orphs mos) (mos_num_norphs mos)
          return $ updatePkgOrphStats pos mos
  -- process and print all modules
  pos' <- foldM f (initPkgOrphStats pid) mods
  liftIO $ putStrLn ""
  liftIO $ putStrLn ""
  return pos'


printSDoc :: SDoc -> Ghc ()
printSDoc sdoc = do
  dflags <- getSessionDynFlags
  liftIO $ putStrLn $ showSDoc dflags sdoc

-- | Process the module and return the number of orphan and non-orphan instances, respectively.
processMod :: Module -> Ghc ModOrphStats
processMod mod = do
  dflags <- getSessionDynFlags
  iface  <- ifaceForMod mod
  let insts = mi_insts iface

  -- TODO: family instances!

  -- partition instances into orphans and non-orphans
  let (orphs, non_orphs) = L.partition (isNothing . ifInstOrph) insts
  let (num_orphs, num_non_orphs) = (length orphs, length non_orphs)

  let mos = ModOrphStats (moduleName mod) (modulePackageId mod) num_orphs num_non_orphs
  when (mosIsOrphan mos) -- print any orphans
       (liftIO $ putStrLn $ showSDoc dflags $ vcat $ mapMaybe pprInstanceIfOrphan insts)

  -- liftIO $ putStrLn $ "  orphans:     " ++ show num_orphs
  -- liftIO $ putStrLn $ "  non-orphans: " ++ show num_non_orphs
  return mos


ifaceForMod :: Module -> Ghc ModIface
ifaceForMod mod = do
  hsc_env <- getSession
  dflags  <- getSessionDynFlags
  mod_loc <- findModLocation mod

  -- computation that reads iface in typechecking monad
  let tcIface = do
        res <- readIface mod (ml_hi_file mod_loc) False
        return $ case res of
          Maybes.Succeeded mod_iface -> mod_iface
          Maybes.Failed _            -> error $ "lookup failed for module " ++ moduleNameString (moduleName mod)

  -- initialize iface computation in typechecking monad and lift to Ghc monad
  liftIO $ initTcForLookup hsc_env tcIface


pprInstanceWithOrphan :: IfaceClsInst -> SDoc
pprInstanceWithOrphan inst = case ifInstOrph inst of
  Just parent -> text "|   " <> ppr (ifInstCls inst) <+> ppr (ifInstTys inst) <+> char '@' <+> ppr parent
  Nothing     -> text "| O " <> ppr (ifInstCls inst) <+> ppr (ifInstTys inst) <+> text " (orphan)"

pprInstanceIfOrphan :: IfaceClsInst -> Maybe SDoc
pprInstanceIfOrphan inst = case ifInstOrph inst of
  Just parent -> Nothing
  Nothing     -> Just $ text "  * " <> ppr (ifInstCls inst) <+> ppr (ifInstTys inst)

    ---------------------------------------------------------



    -- liftIO $ putStrLn "ok"
    -- b <- isLoaded $ mkModuleName "Data.Map.Lazy"
    -- liftIO $ putStrLn $ show b

    --mods <- packageDbModules True
    --liftIO $ putStrLn $ show $ map (showSDoc dflags . pprModule) mods


    -- on laptop, this is complaining about Data.Map.Base being hidden?
    --mod <- lookupModule (mkModuleName "Data.Map.Base") Nothing
    --liftIO $ putStrLn $ showSDoc dflags $ pprModule mod

    -- liftIO $ putStrLn $ showSDoc dflags $ ifaceStats eps

    -- mbInfo <- getModuleInfo mod
    -- let mbIface = mbInfo >>= modInfoIface
    -- -- let mbInsts   = fmap mi_insts mbIface
    -- -- liftIO $ putStrLn $ case mbInsts of
    -- --   Just insts -> showSDoc dflags $ vcat $ map ppr insts
    -- --   Nothing    -> "no instances"
    -- liftIO $ putStrLn $ case mbIface of
    --   Just iface -> "iface" -- showSDoc dflags $ pprModIface iface
    --   Nothing    -> "no iface"



    -- hscEnv <- getSession
    -- eps <- liftIO $ hscEPS hscEnv
    -- let pit = eps_PIT eps
    -- liftIO $ putStrLn $ show $ map (showSDoc dflags . pprModule) $ moduleEnvKeys pit

    --let tcInstances = do
    --      res <- findAndReadIface Outputable.empty mod False
    --      return $ case res of
    --        Maybes.Succeeded (modIface, _) -> Just $ mi_insts modIface
    --        Maybes.Failed _ -> Nothing
    --mbInsts <- liftIO $ initTcForLookup hscEnv tcInstances
    --liftIO $ putStrLn $ case mbInsts of
    --  Just []    -> "no instances"
    --  Just insts -> showSDoc dflags $ vcat $ map ppr insts
    --  Nothing    -> "lookup failed"



type PkgModMap = M.Map PackageId [Module]

currentPkgModMap :: DynFlags -> [String] -> PkgModMap
currentPkgModMap dflags req_pkgs =
    M.fromList $ [ (pkgIdFromIpi ipi, map (mkMod ipi) (allModsFromIpi ipi))
                 | ipi <- ipis
                 , pkgNameFromIpi ipi `elem` req_pkgs
                 ]
  where
    ipis        = eltsUFM $ pkgIdMap $ pkgState dflags
    pkgIdFromIpi ipi   = mkPackageId $ sourcePackageId ipi
    pkgNameFromIpi ipi = pkgIdName $ pkgIdFromIpi ipi
    mkMod ipi modName  = mkModule (pkgIdFromIpi ipi) modName
    allModsFromIpi ipi = (exposedModules ipi) ++ (hiddenModules ipi)
  
pkgIdName :: PackageId -> String
pkgIdName pid = takeWhile (\c -> c /= '-') $ packageIdString pid

findModLocation :: Module -> Ghc ModLocation
findModLocation mod = do
  hsc_env <- getSession
  case getPackageConfig hsc_env mod of
    Nothing -> error $ "cannot find PackageConfig for " ++ (packageIdString (modulePackageId mod))
    Just pkg_conf ->
      let
        dflags = hsc_dflags hsc_env
        tag = buildTag dflags

             -- hi-suffix for packages depends on the build tag.
        package_hisuf | null tag  = "hi"
                      | otherwise = tag ++ "_hi"

        mk_hi_loc = mkHiOnlyModLocation dflags package_hisuf

        import_dirs = importDirs pkg_conf
        -- we never look for a .hi-boot file in an external package;
        -- .hi-boot files only make sense for the home package.
      in
      liftIO $ case import_dirs of
        [one] | MkDepend <- ghcMode dflags -> do
          -- there's only one place that this .hi file can be, so
          -- don't bother looking for it.
          let basename = moduleNameSlashes (moduleName mod)
          mk_hi_loc one basename
        _ ->
          searchPathExts import_dirs mod [(package_hisuf, mk_hi_loc)]

-- | Extract the package config for given module's package out of the current package state.
getPackageConfig :: HscEnv -> Module -> Maybe PackageConfig
getPackageConfig hsc_env mod = lookupPackage pkg_map pkg_id
  where
    dflags = hsc_dflags hsc_env
    pkg_id = modulePackageId mod
    pkg_map = pkgIdMap (pkgState dflags)



type FileExt = String   -- Filename extension
type BaseName = String  -- Basename of file


-- General path searching. Copied from GHC API implementation.
searchPathExts
  :: [FilePath]         -- paths to search
  -> Module             -- module name
  -> [ (
        FileExt,                                -- suffix
        FilePath -> BaseName -> IO ModLocation  -- action
       )
     ]
  -> IO ModLocation
searchPathExts paths mod exts = search to_search
  where
    basename = moduleNameSlashes (moduleName mod)

    to_search :: [(FilePath, IO ModLocation)]
    to_search = [ (file, fn path basename)
                | path <- paths,
                  (ext,fn) <- exts,
                  let base | path == "." = basename
                           | otherwise   = path </> basename
                      file = base <.> ext
                ]

    search [] = error $ "can't find .hi file for " ++ moduleNameString (moduleName mod)
    search ((file, mk_result) : rest) = do
      b <- doesFileExist file
      if b
        then mk_result
        else search rest

----------------------------------------------------------------------------

-- findModLocation :: HscEnv -> Module -> IO FindResult
-- findModLocation hsc_env mod = case getPackageConfig hsc_env mod of
--   Nothing       -> return $ NoPackage $ modulePackageId mod
--   Just pkg_conf ->
--     let
--       dflags = hsc_dflags hsc_env
--       tag = buildTag dflags

--            -- hi-suffix for packages depends on the build tag.
--       package_hisuf | null tag  = "hi"
--                     | otherwise = tag ++ "_hi"

--       mk_hi_loc = mkHiOnlyModLocation dflags package_hisuf

--       import_dirs = importDirs pkg_conf
--       -- we never look for a .hi-boot file in an external package;
--       -- .hi-boot files only make sense for the home package.
--     in
--     case import_dirs of
--       [one] | MkDepend <- ghcMode dflags -> do
--         -- there's only one place that this .hi file can be, so
--         -- don't bother looking for it.
--         let basename = moduleNameSlashes (moduleName mod)
--         loc <- mk_hi_loc one basename
--         return $ Found loc mod
--       _ ->
--         searchPathExts import_dirs mod [(package_hisuf, mk_hi_loc)]



-- -- General path searching. Copied from GHC API implementation.
-- searchPathExts
--   :: [FilePath]         -- paths to search
--   -> Module             -- module name
--   -> [ (
--         FileExt,                                -- suffix
--         FilePath -> BaseName -> IO ModLocation  -- action
--        )
--      ]
--   -> IO FindResult
-- searchPathExts paths mod exts = search to_search
--   where
--     basename = moduleNameSlashes (moduleName mod)

--     to_search :: [(FilePath, IO ModLocation)]
--     to_search = [ (file, fn path basename)
--                 | path <- paths,
--                   (ext,fn) <- exts,
--                   let base | path == "." = basename
--                            | otherwise   = path </> basename
--                       file = base <.> ext
--                 ]

--     search [] = return (NotFound { fr_paths = map fst to_search
--                                  , fr_pkg   = Just (modulePackageId mod)
--                                  , fr_mods_hidden = [], fr_pkgs_hidden = []
--                                  , fr_suggestions = [] })

--     search ((file, mk_result) : rest) = do
--       b <- doesFileExist file
--       if b
--         then do { loc <- mk_result; return (Found loc mod) }
--         else search rest
