module Test where

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

generateGraphL :: Ctx -> IO Bool
generateGraphL ctx = do
  graphToDotPng "dag" $ worldDagExceptExtPkgs ctx "Test1.Left"

generateFullGraphP :: Ctx -> IO Bool
generateFullGraphP ctx = do
  graphToDotPng "prelude-dag" $  worldDag ctx "Prelude"


-- main = generateFullGraphP
