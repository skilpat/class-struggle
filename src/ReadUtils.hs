module ReadUtils where

import Control.Monad
import Data.Char
  ( isDigit, isAlpha )
import Data.List
  ( sortBy, intercalate )
import Data.List.Split
  ( splitOn )
import qualified Data.Map.Strict as M
import System.Directory
import System.Exit
  ( exitFailure )
import System.FilePath
import System.IO
  ( withFile, hIsEOF, IOMode(ReadMode), hGetLine, getContents, stdout, hFlush )

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
import SrcLoc
  ( noLoc )
import Text.Printf
import TcIface
  ( typecheckIface )

import Moduleish


type PkgModMap = M.Map String (PackageId, [Module])

-- | Returns a mapping of all the available packages to the modules that they
--   define (whether exposed or not); the result is partitioned into two
--   such mappings, the first contains only those packages whose names are
--   passed in as the selected packages, and the second contains the rest of
--   the available packages.
currentPkgModMap :: [String] -> Ghc (PkgModMap, PkgModMap)
currentPkgModMap req_pkgs = do
  dflags <- getSessionDynFlags

  -- Installed packages
  let ipis = eltsUFM $ pkgIdMap $ pkgState dflags

  let keep pid | null req_pkgs = True
               | otherwise     = pkgIdName pid `elem` req_pkgs

  -- Generate association list for map
  liftIO $ putStrLn "Reading current package mod map..."
  let mkEntry ipi = do
        let pid = pkgIdFromIpi ipi
        let mods = map (mkMod ipi) (allModsFromIpi ipi)
        let keepstr = if not (null req_pkgs) && keep pid then "> " else "  "
        printSDoc $ text keepstr <> ppr pid <+> parens (int (length mods))
        return (pkgIdName pid, (pid, mods, keep pid))
  assocs <- mapM mkEntry ipis
  let pkg_mod_map_with_keep = M.fromList assocs

  -- Filter out any packages that aren't in the required list.
  let f_select (pid, mods, keep)
        | keep      = Just (pid, mods)
        | otherwise = Nothing
  let pkg_mod_map_selected = M.mapMaybe f_select pkg_mod_map_with_keep

  -- Filter out packages that *are* in the required list.
  let f_unselect (pid, mods, keep)
        | keep      = Nothing
        | otherwise = Just (pid, mods)
  let pkg_mod_map_unselected = M.mapMaybe f_unselect pkg_mod_map_with_keep

  liftIO $ putStrLn "Done reading current package mod map."
  return (pkg_mod_map_selected, pkg_mod_map_unselected)

  where
    pkgIdFromIpi ipi   = mkPackageId $ sourcePackageId ipi
    mkMod ipi modName  = mkModule (pkgIdFromIpi ipi) modName
    allModsFromIpi ipi = (exposedModules ipi) ++ (hiddenModules ipi)
  

-- | Given a PackageId, which looks like "foo-bar-1.2.3", peel off the part
--   before the version number, "foo-bar".
pkgIdName :: PackageId -> String
pkgIdName = fst . pkgIdSplit

pkgIdVersion :: PackageId -> String
pkgIdVersion = snd . pkgIdSplit

pkgIdSplit :: PackageId -> (String, String)
pkgIdSplit pid = (name, version)
  where
    pieces = splitOn "-" (packageIdString pid)
    (npieces, vpieces) = span (isAlpha . head) pieces
    name = intercalate "-" npieces
    version = case vpieces of
      []  -> "" -- for package strings like "base"
      [s] -> s  -- for normal package strings
      _   -> error "unknown package string format"

stringToModule :: String -> Module
stringToModule s = mkModule pid mname
  where
    (p, ':':m) = span (/= ':') s
    pid   = stringToPackageId p
    mname = mkModuleName m


printPkgs :: Ghc ()
printPkgs = do
  dflags <- getSessionDynFlags

  let pkgIdFromIpi ipi   = mkPackageId $ sourcePackageId ipi
  let pkgNameFromIpi ipi = pkgIdName $ pkgIdFromIpi ipi
  let ipis = eltsUFM $ pkgIdMap $ pkgState dflags
  let pids = sortBy stablePackageIdCmp $ map pkgIdFromIpi ipis

  forM_ pids (printSDoc . ppr)  



updateDynFlags :: [String] -> Ghc ()
updateDynFlags str_args = do
  dflags0 <- getSessionDynFlags
  let args = map noLoc str_args
  (dflags, _, _) <- parseDynamicFlagsCmdLine dflags0 args
  pids <- setSessionDynFlags dflags
  -- forM_ pids $ \pid -> do
  --   printSDoc $ text " ! " <> ppr pid
  return ()


printSDoc :: SDoc -> Ghc ()
printSDoc sdoc = do
  dflags <- getSessionDynFlags
  liftIO $ putStrLn $ showSDoc dflags sdoc



-- Flush any buffered output
flush :: Ghc ()
flush = liftIO $ hFlush stdout


type FileExt = String   -- Filename extension
type BaseName = String  -- Basename of file


-- | Find and read the ModIface for the given Moduleish. Basically, if it's
--   a boot file, load the .hi-boot file; otherwise, load the .hi file.
readIfaceForMish :: Moduleish -> Ghc ModIface
readIfaceForMish mish = do
  hsc_env <- getSession

  -- computation that reads iface in typechecking monad
  let tcIface = do
        res <- findAndReadIface (ppr mish) (mish_mod mish) (mish_boot mish)
        return $ case res of
          Maybes.Succeeded (mod_iface, _) -> mod_iface
          Maybes.Failed _ -> error $ "lookup failed for module " ++ show mish

  -- initialize iface computation in typechecking monad and lift to Ghc monad
  liftIO $ initTcForLookup hsc_env tcIface


-- | A loaded ModIface needs to be "typechecked" in order to get its instances
--   in the right format. So do that.
readInstsFromIface :: ModIface -> Ghc [ClsInst]
readInstsFromIface iface = do
  liftIO $ putStrLn $ "+ getting session"
  flush
  hsc_env <- getSession
  liftIO $ putStrLn $ "+ reading " ++ (show $ mkModuleishImpl $ mi_module iface)
  flush
  mod_details <- liftIO $ initTcForLookup hsc_env $ typecheckIface iface
  liftIO $ putStrLn $ "+ done reading " ++ (show $ mkModuleishImpl $ mi_module iface)
  flush
  return $ md_insts mod_details



-- | Extract the package config for given module's package out of the current package state.
getPackageConfig :: HscEnv -> Module -> Maybe PackageConfig
getPackageConfig hsc_env mod = lookupPackage pkg_map pkg_id
  where
    dflags = hsc_dflags hsc_env
    pkg_id = modulePackageId mod
    pkg_map = pkgIdMap (pkgState dflags)



readPkgList :: IO [String]
readPkgList = do
  pkgs_as_lines <- liftM (filter is_pkg . lines) getContents
  case pkgs_as_lines of
    [line] -> return $ words line
    _      -> return pkgs_as_lines
  where
    is_pkg line = not (null line) && head line /= '#'


readSandboxPath :: IO FilePath
readSandboxPath = withFile "cabal.sandbox.config" ReadMode loop
  where
    loop handle = do
      eof <- hIsEOF handle
      when eof $ do
        putStrLn "error: couldn't find package-db"
        exitFailure
      line <- hGetLine handle
      case span (/= ':') line of
        ("package-db", ':':' ':path) -> return path
        _ -> loop handle



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
