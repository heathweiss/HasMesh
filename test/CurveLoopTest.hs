
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CurveLoopTest(runTests) where

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

import qualified Gmsh.CurveLoop as CL

runTests = do
 P.putStrLn $ "=============== Cureve Loop Test ====================="  
 -- ===================================  ===================

 let
  createCurveLoopFrom3Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L.toSafeList3 vertexs 
      points <- runRIO env $ Pnt.toPoints safeVertex
      lines <- runRIO env $ Line.toLines points
      curveLoop <- runRIO env $ CL.toCurveLoop lines
      assertEqual "create curve loop from 3 vertex" [1] (map Env.evalCurveLoopId (L.evalSafeList1 curveLoop))
      
   )
 _ <- runTestTT createCurveLoopFrom3Vertexs

 let
  create2CurveLoopsFrom3Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L.toSafeList3 vertexs 
      points <- runRIO env $ Pnt.toPoints safeVertex
      lines <- runRIO env $ Line.toLines points
      curveLoop1 <- runRIO env $ CL.toCurveLoop lines
      curveLoop2 <- runRIO env $ CL.toCurveLoop lines
      assertEqual "create curve loop from 3 vertex" [2] (map Env.evalCurveLoopId (L.evalSafeList1 curveLoop2))
      
   )
 _ <- runTestTT create2CurveLoopsFrom3Vertexs


 let
  createCurveLoopFrom4Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L.toSafeList3 vertexs 
      points <- runRIO env $ Pnt.toPoints safeVertex
      lines <- runRIO env $ Line.toLines points
      curveLoop <- runRIO env $ CL.toCurveLoop lines
      assertEqual "create curve loop from 3 vertex" [1] (map Env.evalCurveLoopId (L.evalSafeList1 curveLoop))
      
   )
 _ <- runTestTT createCurveLoopFrom4Vertexs
 
 P.putStrLn ""
