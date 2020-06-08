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
import qualified Utils.Environment as Env
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Point as Pts  
import qualified Utils.EnvironmentLoader as EnvLdr 
import qualified Utils.Environment as Env
import qualified Gmsh.Line as Line
import qualified Gmsh.Point as Pnt
import qualified List.Safe3 as L3
import qualified List.Safe1 as L1
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import List.Base((>>+))

import qualified Gmsh.CurveLoop as CL

runTests = do
 P.putStrLn $ "=============== Cureve Loop Test ====================="  
 -- =================================== simple straight forward construction  ===================

 let
  createCurveLoopFrom3Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 vertexs 
      points <- runRIO env $ Pnt.toPoints safeVertex
      lines <- runRIO env $ Line.toLines points
      curveLoop <- runRIO env $ CL.toCurveLoop lines
      assertEqual "create curve loop from 3 vertex" [1] (map Env.evalCurveLoopId (L1.evalSafeList1 curveLoop))
      
   )
 _ <- runTestTT createCurveLoopFrom3Vertexs

 let
  create2CurveLoopsFrom3Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 vertexs 
      points <- runRIO env $ Pnt.toPoints safeVertex
      lines <- runRIO env $ Line.toLines points
      curveLoop1 <- runRIO env $ CL.toCurveLoop lines
      curveLoop2 <- runRIO env $ CL.toCurveLoop lines
      assertEqual "create curve loop from 3 vertex" [2] (map Env.evalCurveLoopId (L1.evalSafeList1 curveLoop2))
      
   )
 _ <- runTestTT create2CurveLoopsFrom3Vertexs


 let
  createCurveLoopFrom4Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 vertexs 
      points <- runRIO env $ Pnt.toPoints safeVertex
      lines <- runRIO env $ Line.toLines points
      curveLoop <- runRIO env $ CL.toCurveLoop lines
      assertEqual "create curve loop from 4 vertex" [1] (map Env.evalCurveLoopId (L1.evalSafeList1 curveLoop))
      
   )
 _ <- runTestTT createCurveLoopFrom4Vertexs

-- ================================================= as instance Utils.Add ====================================
 let
  add2CurveLoopsThatDoNotOverlap = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexes1 <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      points1 <- runRIO env $ Pnt.toPoints safeVertexes1
      lines1 <- runRIO env $ Line.toLines points1
      curveLoop1 <- runRIO env $ CL.toCurveLoop lines1
      safeVertexes2 <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 [Geo.newVertex  11 11 11, Geo.newVertex  22 22 22, Geo.newVertex  33 33 33]
      points2 <- runRIO env $ Pnt.toPoints safeVertexes2
      lines2 <- runRIO env $ Line.toLines points2
      curveLoop2 <- runRIO env $ CL.toCurveLoop lines2
      let
        curveLoopsAdded = pure curveLoop1 >>+ curveLoop2
      assertEqual "the 2 curve loops have diff ids" (Right [1,2])
        (case curveLoopsAdded of
           Right curveLoop ->
            Right $ map Env.evalCurveLoopId (L1.evalSafeList1 curveLoop)
           Left err -> Left err
        )
   )
 _ <- runTestTT add2CurveLoopsThatDoNotOverlap


 let
  addingACurveLoopToItselfFails = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexes1 <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      points1 <- runRIO env $ Pnt.toPoints safeVertexes1
      lines1 <- runRIO env $ Line.toLines points1
      curveLoop1 <- runRIO env $ CL.toCurveLoop lines1
      let
        curveLoopsAdded = pure curveLoop1 >>+ curveLoop1
      assertEqual "the 2 curve loops have diff ids" (Left (Hex.GeneralException "non unique curveloop list"))
        (case curveLoopsAdded of
           Right curveLoop ->
            Right $ map Env.evalCurveLoopId (L1.evalSafeList1 curveLoop)
           Left err -> Left err
        )
   )
 _ <- runTestTT addingACurveLoopToItselfFails

 let
  addCurveLoopsNonUniqueLength3 = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexes1 <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      points1 <- runRIO env $ Pnt.toPoints safeVertexes1
      lines1 <- runRIO env $ Line.toLines points1
      curveLoop1 <- runRIO env $ CL.toCurveLoop lines1
      safeVertexes2 <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 [Geo.newVertex  11 11 11, Geo.newVertex  22 22 22, Geo.newVertex  33 33 33]
      points2 <- runRIO env $ Pnt.toPoints safeVertexes2
      lines2 <- runRIO env $ Line.toLines points2
      curveLoop2 <- runRIO env $ CL.toCurveLoop lines2
      let
        nonUniqueCurveLoops = pure curveLoop1 >>+ curveLoop2 >>+ curveLoop2
      assertEqual "the 2nd and 3rd curve loops are not unique" (Left (Hex.GeneralException "non unique curveloop list"))
        (case nonUniqueCurveLoops of
           Right curveLoop ->
            Right $ map Env.evalCurveLoopId (L1.evalSafeList1 curveLoop)
           Left err -> Left err
        )
   )
 _ <- runTestTT addCurveLoopsNonUniqueLength3


 
 P.putStrLn ""
