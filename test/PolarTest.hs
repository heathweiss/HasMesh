{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PolarTest(runTests) where

import RIO
import Test.HUnit
import qualified Prelude as P
import qualified Geometry.Polar as Polar
import qualified Geometry.Vertex as V
import qualified Geometry.Axis as Axis
import qualified Data.Hashable as H
import qualified Utils.EnvironmentLoader as EnvLdr
import qualified Utils.RunExceptions as HexR
import qualified Gmsh.Gmsh as Gmsh
import qualified Utils.Environment as Env
import qualified Data.Bifunctor as Bif

runTests = do
 P.putStrLn $ "=============== Polar Test ====================="  


 let
  createVertexInEachQuadrant = TestCase $ assertEqual
   "create a vetex in middle of each quadrant angle"
   [V.newVertex ( 1)   ( 1.0) 0,
    V.newVertex (-1.0)   1    0,
    V.newVertex (-1.0) (-1)   0,
    V.newVertex ( 1) (  -1.0) 0
   ]
   (  Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
       
       [(45, sqrt 2),
        (135,sqrt 2),
        (225,sqrt 2),
        (315,sqrt 2)
       ]
   ) 
 runTestTT createVertexInEachQuadrant

 let
  createVertexInEachQuadrantWithShiftedOrigin = TestCase $ assertEqual
   "create a vetex in middle of each quadrant angle, but with x,y, and z all shifted by 10 "
   [V.newVertex ( 11)   ( 11.0) 10,
    V.newVertex ( 9.0)   11    10,
    V.newVertex (9.0) (9)   10,
    V.newVertex ( 11) (  9.0) 10
   ]
   (  Polar.newVertexes (Axis.XAxis 10) (Axis.YAxis 10)(Axis.ZAxis 10)
       
       [(45, sqrt 2),
        (135,sqrt 2),
        (225,sqrt 2),
        (315,sqrt 2)
       ]
   ) 
 runTestTT createVertexInEachQuadrantWithShiftedOrigin

 P.putStrLn ""
 
