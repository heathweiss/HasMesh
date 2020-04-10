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
import qualified Gmsh.Point as Pnt
import qualified Utils.List as L
import qualified Utils.RunExceptions as HexR

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

-- ======================================== create and increment Line Id's: use with Env ================================
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

-- ============================ create Lines from [Vertex] ===================================
-- create 2 vertex, get their gmsh ids, then create a new line from those ids.
 let
  testCreateLineFromVertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      
      point1 <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      point2 <- runRIO env $ Pts.toPoint $ Geo.newVertex  4 5 6
      lineId <- runRIO env $ Line.createLineFromPoints point1 point2
      --runSimpleApp $ logInfo $ displayShow lineId
      assertEqual "create line from 2 point ids" (Gmsh.LineId $ Gmsh.LineInt 1) lineId
   )
 runTestTT testCreateLineFromVertexs


-- create 2 vertex, get their gmsh ids, then create a new line from those ids as a [id].
-- Note that there are 3 lines from only 3 vertex. This is because it closes the loop by creating a line from the last vertex back to the head vertex.
 let
  testCreateLineFromVertexs2 = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      {-
      point1 <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      point2 <- runRIO env $ Pts.toPoint $ Geo.newVertex  4 5 6
      point3 <- runRIO env $ Pts.toPoint $ Geo.newVertex  4.7 4.8 4.9-}
      let vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6, Geo.newVertex  7 8 9]
      eitherPoints <- runRIO env $ Pnt.toPoints vertexs
      points <- HexR.runEitherIO "points" eitherPoints
      --lineId <- runRIO env $ Line.createLinesFromPoints $ L.Cons point1 point2 [point3] L.Nil
      lineIds <- runRIO env $ Line.createLinesFromPoints points
      assertEqual "create line from 2 point ids" ([Gmsh.LineId $ Gmsh.LineInt 1, Gmsh.LineId $ Gmsh.LineInt 2, Gmsh.LineId $ Gmsh.LineInt 3]) lineIds
   )
 runTestTT testCreateLineFromVertexs2

{-
 let
  testCreateLineFromVertexs2 = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      
      point1 <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      point2 <- runRIO env $ Pts.toPoint $ Geo.newVertex  4 5 6
      point3 <- runRIO env $ Pts.toPoint $ Geo.newVertex  4.7 4.8 4.9
      lineId <- runRIO env $ Line.createLinesFromPoints $ L.Cons point1 point2 [point3] L.Nil
      assertEqual "create line from 2 point ids" ([Gmsh.LineId $ Gmsh.LineInt 1, Gmsh.LineId $ Gmsh.LineInt 2, Gmsh.LineId $ Gmsh.LineInt 3]) lineId
   )
 runTestTT testCreateLineFromVertexs2

-}
