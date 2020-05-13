{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PlaneSurfaceTest(runTests) where

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
import qualified List.Safe1 as L1
import qualified List.Safe3 as L3
import List.Base((>>+))
import qualified Utils.RunExceptions as HexR
import qualified Gmsh.CurveLoop as CL
import qualified Gmsh.PlaneSurface as PS

runTests = do
 P.putStrLn $ "=============== PlaneSurface Tests ====================="
 let
  createPlaneSurfaceFrom3Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 vertexs 
      points <- runRIO env $ Pnt.toPoints safeVertex
      lines <- runRIO env $ Line.toLines points
      curveLoop <- runRIO env $ CL.toCurveLoop lines
      planeSurface <- runRIO env $ PS.toPlaneSurface curveLoop
      assertEqual
        "create plane surface from 3 vertex"
        ([1],[1])
        (map Env.evalPlaneSurfaceId (L1.evalSafeList1 planeSurface)  ,map Env.evalCurveLoopId (L1.evalSafeList1 curveLoop))
      
   )
 _ <- runTestTT createPlaneSurfaceFrom3Vertexs

 let
  createPlaneSurfaceFrom2CurveLoops = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs1 = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex1 <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 vertexs1 
      curveLoop1 <- runRIO env $ Pnt.toPoints safeVertex1 >>= Line.toLines >>= CL.toCurveLoop
      let vertexs2 = [Geo.newVertex  11 11 11, Geo.newVertex  22 22 22, Geo.newVertex  33 33 33]
      safeVertex2 <-  HexR.runEitherIO "safeVertex" $ L3.toSafeList3 vertexs2 
      curveLoop2 <- runRIO env $ Pnt.toPoints safeVertex2 >>= Line.toLines >>= CL.toCurveLoop
      curveLoops <- HexR.runEitherIO "curveloops" $ Right curveLoop1 >>+ curveLoop2
      planeSurface <- runRIO env $ PS.toPlaneSurface curveLoops 
      assertEqual
        "create plane surface from 2 curve loops" 
        ([1],[1,2])
        (
          map Env.evalPlaneSurfaceId (L1.evalSafeList1 planeSurface),
          map Env.evalCurveLoopId (L1.evalSafeList1 curveLoops)
        )
      
   )
 _ <- runTestTT createPlaneSurfaceFrom2CurveLoops


 let
  createPlaneSurfaceFrom3CurveLoops = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs1 = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex1 <-  HexR.runEitherIO "safeVertex1" $ L3.toSafeList3 vertexs1 
      curveLoop1 <- runRIO env $ Pnt.toPoints safeVertex1 >>= Line.toLines >>= CL.toCurveLoop
      let vertexs2 = [Geo.newVertex  11 11 11, Geo.newVertex  22 22 22, Geo.newVertex  33 33 33]
      safeVertex2 <-  HexR.runEitherIO "safeVertex2" $ L3.toSafeList3 vertexs2 
      curveLoop2 <- runRIO env $ Pnt.toPoints safeVertex2 >>= Line.toLines >>= CL.toCurveLoop
      let vertexs3 = [Geo.newVertex  111 111 111, Geo.newVertex  222 222 222, Geo.newVertex  333 333 333]
      safeVertex3 <-  HexR.runEitherIO "safeVertex3" $ L3.toSafeList3 vertexs3 
      curveLoop3 <- runRIO env $ Pnt.toPoints safeVertex3 >>= Line.toLines >>= CL.toCurveLoop
      curveLoops <- HexR.runEitherIO "curveLoops" $ Right curveLoop1 >>+ curveLoop2 >>+ curveLoop3
      planeSurface <- runRIO env $ PS.toPlaneSurface curveLoops 
      assertEqual
        "create plane surface from 3 curve loops" 
        ([1],[1,2,3])
        (
          map Env.evalPlaneSurfaceId (L1.evalSafeList1 planeSurface),
          map Env.evalCurveLoopId (L1.evalSafeList1 curveLoops)
        )
      
   )
 _ <- runTestTT createPlaneSurfaceFrom3CurveLoops


 P.putStrLn ""

