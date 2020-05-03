{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}


module LineTest(runTests) where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO

import qualified Geometry.Vertex as V
--import qualified Gmsh.Gmsh as Gmsh
import qualified Utils.Environment as Env
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Point as Pts  
import qualified Utils.EnvironmentLoader as EnvLdr 
import qualified Utils.Environment as Env
import qualified Gmsh.Line as Line
import qualified Gmsh.Point as Pnt
import qualified Utils.List as L
import qualified Utils.RunExceptions as HexR

runTests = do
 P.putStrLn $ "=============== LineTest ====================="
 
--Load an environment in IO, and then get 2 LineId's from within a RIO monad.
--By getting > 1 Id, will also test that the LineId supply was incremented when getting the 1st id.
 let
  getASecondLineId = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      _ <- runRIO env Env.getLineId
      secondId <- runRIO env Env.getLineId
      assertEqual "get the vector id from an ioref" 2 ((\(Env.LineId (Env.LineInt' int)) -> int )secondId)
   )
 _ <- runTestTT getASecondLineId
 
 let
  createLinesFrom3Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L.toSafeList3 vertexs 
      --points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points"
      points <- runRIO env $ Pnt.toPoints safeVertex
      lineIds <- runRIO env $ Line.toLines points
      assertEqual "create line from 2 point ids" [1, 2, 3] (map Env.evalLineId (L.evalSafeList3 lineIds))
   )
 _ <- runTestTT createLinesFrom3Vertexs
 
 let
  createLinesFrom4Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      --let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4]
      safeVertex <- HexR.runEitherIO "safeVertex"  $ L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4]
      points <- runRIO env $ Pnt.toPoints safeVertex 
      --points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" 
      lineIds <- runRIO env $ Line.toLines points
      assertEqual "create line from 2 point ids" [1, 2, 3, 4] (map Env.evalLineId (L.evalSafeList3 lineIds))
   )
 runTestTT createLinesFrom4Vertexs

 let
  createLinesFrom6Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      --let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4, Geo.newVertex 5 5 5, Geo.newVertex 6 6 6]
      safeVertex <- HexR.runEitherIO "safeVertex"  $ L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4, Geo.newVertex 5 5 5, Geo.newVertex 6 6 6]
      points <- runRIO env $ Pnt.toPoints safeVertex 
      --points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" 
      lineIds <- runRIO env $ Line.toLines points
      assertEqual "create line from 2 point ids" [1, 2, 3, 4, 5, 6] (map Env.evalLineId (L.evalSafeList3 lineIds))
   )
 runTestTT createLinesFrom6Vertexs
 
 
--------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------- use new toPoints with single enty into safelist------------------------------------------------
