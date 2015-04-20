module Test where

import System.IO.Unsafe


import Moduleish
import ReadUtils
import ReadWorlds
import World
import WorldCtx
import WorldDag

sandbox_path = "/var/projects/orphans/.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d"

ctx1_ :: IO Ctx
ctx1_ = buildCtx sandbox_path ["base", "test-pkg"]

mishP, mishL :: Moduleish
mishP = mkModuleishFull "base:Prelude"
mishL = mkModuleishFull "test-pkg-0.1.0.0:Test1.Left"

