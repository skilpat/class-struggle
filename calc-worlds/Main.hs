module Main where

import System.Environment
  ( getArgs )

import ReadUtils
import ReadWorlds
import WorldCtx


-- | Given a list of packages as command-line arguments, read all the worlds
--   for these packages and print them out.
main :: IO ()
main = do
  sandbox <- readSandboxPath
  pkgs <- getArgs
  ctx <- buildCtx sandbox pkgs
  printCtx [""] True ctx
