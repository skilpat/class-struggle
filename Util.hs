module Util where

import Control.Monad
import qualified Data.Map as M
import System.Directory
import System.FilePath

import GHC
import Module
import DynFlags
import UniqFM
import PackageConfig
import Packages
import Outputable
import TcRnMonad

import DynFlags
import Finder
import GHC.Paths
-- import GhcMonad(liftIO)
import HscTypes
import IfaceSyn
import LoadIface
import Maybes
import Module
import Text.Printf


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


printSDoc :: SDoc -> Ghc ()
printSDoc sdoc = do
  dflags <- getSessionDynFlags
  liftIO $ putStrLn $ showSDoc dflags sdoc




type FileExt = String   -- Filename extension
type BaseName = String  -- Basename of file


ifaceForMod :: Module -> Bool -> Ghc ModIface
ifaceForMod mod is_boot = do
  hsc_env <- getSession
  dflags  <- getSessionDynFlags
  mod_loc <- findModLocation mod
  printSDoc $ text "mod_loc:" <+> ppr mod_loc

  -- computation that reads iface in typechecking monad
  let tcIface = do
        res <- readIface mod (ml_hi_file mod_loc) is_boot
        return $ case res of
          Maybes.Succeeded mod_iface -> mod_iface
          Maybes.Failed _            -> error $ "lookup failed for module " ++ moduleNameString (moduleName mod)

  -- initialize iface computation in typechecking monad and lift to Ghc monad
  liftIO $ initTcForLookup hsc_env tcIface



-- | Extract the package config for given module's package out of the current package state.
getPackageConfig :: HscEnv -> Module -> Maybe PackageConfig
getPackageConfig hsc_env mod = lookupPackage pkg_map pkg_id
  where
    dflags = hsc_dflags hsc_env
    pkg_id = modulePackageId mod
    pkg_map = pkgIdMap (pkgState dflags)







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
