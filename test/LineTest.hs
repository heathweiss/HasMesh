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
 
--Load an environment in IO, and then get 2 LineId's from within a RIO monad.
--By getting > 1 Id, will also test that the LineId supply was incremented when getting the 1st id.
 let
  testGetAndIncrLineIdFromEnv2 = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      _ <- runRIO env Gmsh.newLineId
      sndId <- runRIO env Gmsh.newLineId
      assertEqual "get the vector id from an ioref" (Gmsh.LineId $ Gmsh.LineInt 2) sndId
   )
 _ <- runTestTT testGetAndIncrLineIdFromEnv2
 

-- ============================ create Lines from [Vertex] ===================================
-- create 2 vertex, get their gmsh ids, then create a new line from those ids as a [id].
-- Note that there are 3 lines from only 3 vertex. This is because it closes the loop by creating a line from the last vertex back to the head vertex.
 let
  createLineFrom3Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" 
      lineIds <- runRIO env $ Line.createLinesFromPoints points
      assertEqual "create line from 2 point ids" ([Gmsh.LineId $ Gmsh.LineInt 1, Gmsh.LineId $ Gmsh.LineInt 2, Gmsh.LineId $ Gmsh.LineInt 3]) (L.evalSafeList3 lineIds)
   )
 runTestTT createLineFrom3Vertexs

 let
  createLineFrom4Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4]
      points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" 
      lineIds <- runRIO env $ Line.createLinesFromPoints points
      assertEqual "create line from 2 point ids"
        ([Gmsh.LineId $ Gmsh.LineInt 1, Gmsh.LineId $ Gmsh.LineInt 2, Gmsh.LineId $ Gmsh.LineInt 3, Gmsh.LineId $ Gmsh.LineInt 4])
        (L.evalSafeList3 lineIds)
   )
 runTestTT createLineFrom4Vertexs

 let
  createLineFrom5Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4, Geo.newVertex 5 5 5]
      points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" 
      lineIds <- runRIO env $ Line.createLinesFromPoints points
      assertEqual "create line from 2 point ids"
        ([Gmsh.LineId $ Gmsh.LineInt 1, Gmsh.LineId $ Gmsh.LineInt 2, Gmsh.LineId $ Gmsh.LineInt 3, Gmsh.LineId $ Gmsh.LineInt 4, Gmsh.LineId $ Gmsh.LineInt 5])
        (L.evalSafeList3 lineIds)
   )
 runTestTT createLineFrom5Vertexs


 let
  createLinesFrom6Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex 1 1 1, Geo.newVertex 2 2 2, Geo.newVertex 3 3 3,
                     Geo.newVertex 4 4 4, Geo.newVertex 5 5 5, Geo.newVertex 6 6 6
                    ]
      points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" 
      lineIds <- runRIO env $ Line.createLinesFromPoints points
      assertEqual "ensures that createSafeListOfLinesFromPoints' closes the lines "
        ([Gmsh.LineId $ Gmsh.LineInt 1, Gmsh.LineId $ Gmsh.LineInt 2, Gmsh.LineId $ Gmsh.LineInt 3, Gmsh.LineId $ Gmsh.LineInt 4, Gmsh.LineId $ Gmsh.LineInt 5, Gmsh.LineId $ Gmsh.LineInt 6])
        (L.evalSafeList3 lineIds)
   )
 runTestTT createLinesFrom6Vertexs


