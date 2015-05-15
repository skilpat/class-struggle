module Main where

import ReadOrphans
  ( stats )
import ReadUtils
  ( readSandboxPath, readPkgList )


-- | Given a list of packages from STDIN, read all the orphans
--   for these packages and print out the stats.
main = do
  sandbox <- readSandboxPath
  pkgs <- readPkgList
  stats sandbox pkgs

