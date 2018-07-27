module Main where

import Data.List
  ( delete )
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
  let modsToPrint = delete "--islands" args

  ctx <- buildCtx sandbox pkgs

  -- print only the selected packages
  printCtx pkgs modsToPrint printIslands ctx
