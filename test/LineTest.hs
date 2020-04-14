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
   (Gmsh.initializeIdLineInt 1)
   (Gmsh.initializeIdLineInt 1) 
 runTestTT testNewLineId


 let
  testIncrLineId = TestCase $ assertEqual
   "Incr a line id"
   (Gmsh.initializeIdLineInt 2)
   (Gmsh.incr $ Gmsh.initializeIdLineInt 1)
   --(Gmsh.evalLineId (Gmsh.incr $ Gmsh.LineId $ Gmsh.initializeIdLineInt 1) )
 runTestTT testIncrLineId

-- ======================================== create and increment Line Id's: use with Env ================================
--Load an environment in IO, and increment the LineId from within a RIO monad.
--This is the inner working of the Gmsh.getLineId fx.
 let
  testGetAndIncrLineIdFromEnv = TestCase
   (do
      let
        workInRIO :: (Enviro.HasIdSupply env) => RIO env (Gmsh.Id (Gmsh.LineInt))
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
      fstId <- runRIO env Gmsh.newLineId
      sndId <- runRIO env Gmsh.newLineId
      assertEqual "get the vector id from an ioref" (Gmsh.LineId $ Gmsh.LineInt 2) sndId
   )
 runTestTT testGetAndIncrLineIdFromEnv2

-- ============================ create Lines from [Vertex] ===================================



-- create 2 vertex, get their gmsh ids, then create a new line from those ids as a [id].
-- Note that there are 3 lines from only 3 vertex. This is because it closes the loop by creating a line from the last vertex back to the head vertex.
 let
  testCreateLineFromVertexs2 = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6, Geo.newVertex  7 8 9]
      eitherPoints <- runRIO env $ Pnt.toPoints vertexs
      points <- HexR.runEitherIO "points" eitherPoints
      lineIds <- runRIO env $ Line.createLinesFromPoints points
      assertEqual "create line from 2 point ids" ([Gmsh.LineId $ Gmsh.LineInt 1, Gmsh.LineId $ Gmsh.LineInt 2, Gmsh.LineId $ Gmsh.LineInt 3]) (L.evalSafeList3 lineIds)
   )
 runTestTT testCreateLineFromVertexs2


