module Main where

import System.Environment
  ( getArgs )

import ReadUtils
  ( readSandboxPath, readPkgList )
import ReadWorlds
  ( buildCtx )
import WorldCtx
  ( printCtx )

-- | Given a list of packages from STDIN, read all the worlds
--   for these packages and print them out. If the `--islands` command-line
--   option is given, then the output will additionally show the islands within
--   each package's world.
main :: IO ()
main = do
  sandbox <- readSandboxPath
  pkgs <- readPkgList

  args <- getArgs
  let printIslands = "--islands" `elem` args

  ctx <- buildCtx sandbox pkgs
  printCtx [""] printIslands ctx
