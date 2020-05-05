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
import qualified Utils.List as L
import qualified Utils.RunExceptions as HexR
import qualified Gmsh.CurveLoop as CL
import qualified Gmsh.PlaneSurface as PS

runTests = do
 P.putStrLn $ "=============== PlaneSurface Tests ====================="
 let
  testSeparator = TestCase $ assertEqual
   "make some damn tests"
   True
   False
 runTestTT testSeparator

 let
  createPlaneSurfaceFrom3Vertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertex <-  HexR.runEitherIO "safeVertex" $ L.toSafeList3 vertexs 
      points <- runRIO env $ Pnt.toPoints safeVertex
      lines <- runRIO env $ Line.toLines points
      curveLoop <- runRIO env $ CL.toCurveLoop lines
      planeSurface <- runRIO env $ PS.toPlaneSurface curveLoop
      assertEqual "create plane surface from 3 vertex" [1] (map Env.evalCurveLoopId (L.evalSafeList1 curveLoop))
      
   )
 _ <- runTestTT createPlaneSurfaceFrom3Vertexs


 P.putStrLn ""
