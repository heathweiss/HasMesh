{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PointTest(runTests) where
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
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex


runTests = do
 P.putStrLn $ "=============== PointTest ====================="  


--Load an environment in IO, then create 3 lines from 3 Vertexs that are in an array, using Pts.toPoints as an intermediate step
 let
  use3VertexToCreate3Points = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6, Geo.newVertex  7 8 9]
      points <- runRIO env $ Pts.toPoints vertexs
      assertEqual "get the vector id from an ioref" (Right $ L.Cons (Gmsh.PointId $ Gmsh.PointInt 1) (Gmsh.PointId $ Gmsh.PointInt 2) (Gmsh.PointId $ Gmsh.PointInt 3)  [(Gmsh.PointId $ Gmsh.PointInt 1)]  L.Nil) points -- result 
   )
 runTestTT use3VertexToCreate3Points

--Pts.toPoints will return an exception if the lenght [vertex] < 3
--toDo: create a PointsTest and move this there. Then create tests for [0 vertex] [1 vertex]
 let
  toPointsFailsWith2Vertex = TestCase
   (do
      
      
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6]
      points <- runRIO env $ Pts.toPoints vertexs
      assertEqual "get the vector id from an ioref" (Left (Hex.SafeList3MinError "length == 2")) points -- result 
   )
 runTestTT toPointsFailsWith2Vertex

