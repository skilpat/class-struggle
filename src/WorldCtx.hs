module WorldCtx where

import Control.Monad.State hiding (liftIO)
import Data.Maybe
  ( catMaybes )
import Data.Monoid
import qualified Data.Set as S


import DynFlags
  ( defaultFatalMessager, defaultFlushOut )
import GHC
  ( Ghc, runGhc, defaultErrorHandler )
import GHC.Paths
  ( libdir )
import HscTypes
import Module
import Outputable
import UniqFM

import Moduleish
import ReadUtils
import World




-- | A context that maps a Moduleish to its typechecked interface and world.
data Ctx = Ctx { ctx_map     :: UniqFM (Moduleish, ModIface, World)
               , ctx_pkg_map :: UniqFM (PackageId, World)
               , ctx_pkgs    :: S.Set PackageId }

instance Monoid Ctx where
  mempty = Ctx { ctx_map     = emptyUFM
               , ctx_pkg_map = emptyUFM
               , ctx_pkgs    = S.empty }

  mappend Ctx { ctx_map  = cmap1, ctx_pkg_map = pmap1, ctx_pkgs = pkgs1 }
          Ctx { ctx_map  = cmap2, ctx_pkg_map = pmap2, ctx_pkgs = pkgs2 } =
    Ctx { ctx_map     = plusUFM cmap1 cmap2
        , ctx_pkg_map = plusUFM pmap1 pmap2
        , ctx_pkgs    = S.union pkgs1 pkgs2 }


-- | A wrapper around the Ghc monad that keeps up with a Ctx as state.
type CtxM a = StateT Ctx Ghc a


lookupWorld :: Ctx -> Moduleish -> Maybe World
lookupWorld Ctx {ctx_map = cmap} mish = do
  (_, _, w) <- lookupUFM cmap mish
  return w
-- lookupWorld ctx mish =
--   case lookupEntriesMatching ctx (show mish) of
--     [(_, _, w)] -> Just w
--     _           -> Nothing


lookupPkgWorld :: Ctx -> PackageId -> Maybe World
lookupPkgWorld Ctx {ctx_pkg_map = pmap} pid = do
  (_, w) <- lookupUFM pmap pid
  return w
-- lookupPkgWorld ctx pid =
--   case lookupPkgEntriesMatching ctx (packageIdString pid) of
--     [(_, w)] -> Just w
--     _        -> Nothing

lookupEntriesMatching :: Ctx -> String -> [(Moduleish, ModIface, World)]
lookupEntriesMatching Ctx {ctx_map = cmap} mod_str =
  [(m,i,w) | (m,i,w) <- eltsUFM cmap
           , show m == mod_str || mishModStr m == mod_str ]

lookupPkgEntriesMatching :: Ctx -> String -> [(PackageId, World)]
lookupPkgEntriesMatching Ctx {ctx_pkg_map = pmap} pkg_str =
  [(p,w) | (p,w) <- eltsUFM pmap
         , packageIdString p == pkg_str || pkgIdName p == pkg_str ]



updateCtxMap :: Moduleish -> ModIface -> World -> CtxM ()
updateCtxMap mish iface w = do
  ctx@Ctx{ctx_map = cmap, ctx_pkgs = cpids} <- get
  -- Update context's map with this Moduleish and (ModIface, World).
  put $ ctx { ctx_map  = addToUFM cmap mish (mish, iface, w)
            , ctx_pkgs = S.union cpids (coveredPkgs w) } -- and depended-upon pkgs


updateCtxPkg :: PackageId -> World -> CtxM ()
updateCtxPkg pid w = do
 ctx@Ctx{ctx_pkg_map = pmap} <- get
 -- Update context's pkg world map with this one.
 put $ ctx { ctx_pkg_map = addToUFM pmap pid (pid, w) }


--- PRINTING -----------------

instance Outputable Ctx where
  --ppr ctx = text "ctx saw packages:" <+> ppr (uniqSetToList (ctx_pkgs ctx))
  --          $+$ pprCtxEntries ctx []
  ppr ctx = pprCtxEntries ctx []


pprCtxEntries :: Ctx -> [String] -> SDoc
pprCtxEntries ctx mods_to_print = sep $ catMaybes maybeEntryDocs ++ pkgEntryDocs
  where
    maybeEntryDocs = [ pprEntry mish w | (mish, _, w) <- eltsUFM (ctx_map ctx) ]
    
    pprEntry mish w
      | shouldPrint mish = Just $ ppr mish <+> text "->" <+> pprIslands (w_wimap w)
      | otherwise        = Nothing

    shouldPrint mish
      | null mods_to_print                   = True
      | mishModStr mish `elem` mods_to_print = True
      | otherwise                            = False

    pkgEntryDocs = [ ppr pid <+> text "=>" <+> pprIslands (w_wimap w)
                   | (pid, w) <- eltsUFM (ctx_pkg_map ctx) ]


-- | Given a list of module names and a Ctx, print out the World of any modules
--   whose names occur in the list.
printCtx :: [String] -> Ctx -> IO ()
printCtx mods ctx = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  runGhc (Just libdir) $ printSDoc $ pprCtxEntries ctx mods

