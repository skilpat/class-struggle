module Test where

import Moduleish
import ReadUtils
import ReadWorlds
import World
import WorldCtx
import WorldDagNew


ctx1_ :: IO Ctx
ctx1_ = readSandboxPath >>= \sandbox -> buildCtx sandbox ["base", "test-pkg"]

mishP, mishL :: Moduleish
mishP = mkModuleishFull "base:Prelude"
mishL = mkModuleishFull "test-pkg-0.1.0.0:Test1.Left"

generateGraphL :: Ctx -> IO Bool
generateGraphL ctx = do
  let Just w = lookupWorld ctx mishL
  putStrLn "Generating graph of Test1.Left..."
  graphToDotPng "dag" $ worldDag w

generateFullGraphP :: Ctx -> IO Bool
generateFullGraphP ctx = do
  let Just w = lookupWorld ctx mishP
  putStrLn "Generating graph of Prelude..."
  graphToDotPng "prelude-dag" $ worldDag w

-- main = generateFullGraphP
