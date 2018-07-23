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
import TcRnMonad
import Text.Printf
import UniqFM
-- import Unsafe.Coerce

import Moduleish
import ReadUtils

-- | Read statistics about orphan instances in installed packages, given an
--   absolute path to a cabal sandbox conf and a list of package names
--   (minus versions) to check.
stats :: String -> [String] -> IO TotalOrphStats
stats sandbox_path req_pkgs = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
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

    (pkg_mod_map_selected, _) <- currentPkgModMap req_pkgs
    processAllPkgs pkg_mod_map_selected


processAllPkgs :: PkgModMap -> Ghc TotalOrphStats
processAllPkgs pkg_mod_map = do
  dflags <- getSessionDynFlags

  -- :: M.Map String PkgOrphStats
  pkg_orph_stats_map <- T.sequence $ M.mapWithKey processPkg pkg_mod_map

  liftIO $ putStrLn "========= TOTALS ========="
  liftIO $ putStrLn ""
  let calcTotalStats :: (Int, TotalOrphStats) -> PkgOrphStats -> Ghc (Int, TotalOrphStats)
      calcTotalStats (ix, tos) pos = do
        liftIO $ putStrLn $ showSDoc dflags $ int ix <> char ')' <+> ppr pos
        liftIO $ putStrLn ""
        return $ (ix+1, updateTotalOrphStats tos pos)
        -- liftIO $ putStrLn $ showSDoc dflags $ ppr pid
        -- liftIO $ putStrLn $ "  orphan modules:   " ++ show num_orphs
        -- liftIO $ putStrLn $ "  orphan instances: " ++ show num_orphs

  -- :: TotalOrphStats
  (_, tos_final) <- F.foldlM calcTotalStats (1, initTotalOrphStats) pkg_orph_stats_map
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

processPkg :: String -> (PackageId, [Module]) -> Ghc PkgOrphStats
processPkg pname (pid, mods) = do
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


-- | Process the module and return the number of orphan and non-orphan instances, respectively.
processMod :: Module -> Ghc ModOrphStats
processMod mod = do
  dflags <- getSessionDynFlags
  iface  <- readIfaceForMish $ mkModuleishImpl mod
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

