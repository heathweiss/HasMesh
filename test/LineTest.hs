{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LineTest(runTests) where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO

import qualified Geometry.Vertex as V
import qualified Gmsh.Gmsh as Gmsh
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Point as Pts  
import qualified Utils.EnvironmentLoader as EnvLdr
import qualified Utils.Environment as Enviro
import qualified Gmsh.Line as Line

runTests = do
 P.putStrLn $ "=============== LineTest ====================="  
 -- =================================== create and increment Line Id's ===================
 let
  testNewLineId = TestCase $ assertEqual
   "Create a new line id"
   (Gmsh.newLineId 1)
   (Gmsh.newLineId 1) 
 runTestTT testNewLineId


 let
  testIncrLineId = TestCase $ assertEqual
   "Incr a line id"
   (Gmsh.newLineId 2)
   (Gmsh.incr $ Gmsh.newLineId 1)
   --(Gmsh.evalLineId (Gmsh.incr $ Gmsh.LineId $ Gmsh.newLineId 1) )
 runTestTT testIncrLineId

-- ======================================== use with Env ================================
--Load an environment in IO, and increment the LineId from within a RIO monad.
--This is the inner working of the Gmsh.getLineId fx.
 let
  testGetAndIncrLineIdFromEnv = TestCase
   (do
      let
        workInRIO :: (Enviro.HasLineIdSupply env) => RIO env (Gmsh.Id (Gmsh.LineInt))
        workInRIO = do
          lineIdRef <- view Enviro.lineIdSupplyL
          currId <- readIORef lineIdRef
          writeIORef lineIdRef (Gmsh.incr currId )
          finalId <- readIORef lineIdRef
          return (finalId)
      
      env <- EnvLdr.loadEnvironment
      result <- runRIO env workInRIO 
      assertEqual "get the vector id from an ioref" (Gmsh.LineId $ Gmsh.LineInt 2) result 
   )
 runTestTT testGetAndIncrLineIdFromEnv

--Load an environment in IO, and then get 2 LineId's from within a RIO monad.
--By getting > 1 Id, will also test that the LineId supply was incremented when getting the 1st id.
 let
  testGetAndIncrLineIdFromEnv2 = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      fstId <- runRIO env Gmsh.getLineId
      sndId <- runRIO env Gmsh.getLineId
      assertEqual "get the vector id from an ioref" (Gmsh.LineId $ Gmsh.LineInt 2) sndId
   )
 runTestTT testGetAndIncrLineIdFromEnv2


-- create 2 vertex, get their gmsh ids, then create a new line from those ids.

 let
  testGetAndIncrLineIdFromEnv3 = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      
      point1 <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      point2 <- runRIO env $ Pts.toPoint $ Geo.newVertex  4 5 6
      lineId <- runRIO env $ Line.createLineFromPoints point1 point2
      --runSimpleApp $ logInfo $ displayShow lineId
      assertEqual "create line from 2 point ids" (Gmsh.LineId $ Gmsh.LineInt 1) lineId
   )
 runTestTT testGetAndIncrLineIdFromEnv3

-- why isn't the Line script printing to stdout, even though the points print.

{-
=============== LineTest =====================
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Cases: 1  Tried: 1  Errors: 0  Failures: 0
Cases: 1  Tried: 1  Errors: 0  Failures: 0

-}
