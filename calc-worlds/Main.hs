module Main where

import ReadUtils
  ( readSandboxPath, readPkgList )
import ReadWorlds
  ( buildCtx )
import WorldCtx
  ( printCtx )


-- | Given a list of packages from STDIN, read all the worlds
--   for these packages and print them out.
main :: IO ()
main = do
  sandbox <- readSandboxPath
  pkgs <- readPkgList
  ctx <- buildCtx sandbox pkgs
  printCtx [""] False ctx
