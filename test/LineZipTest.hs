{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module LineZipTest(runTests) where
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
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import qualified Gmsh.LineZip as LZ
import qualified Geometry.Axis as Axis
import qualified Geometry.Polar as Polar

runTests = do
 P.putStrLn $ "=============== Line Zip Test ====================="

 let
  zipTriangleAlignedWithinTriangle = TestCase
   (do
      let
        createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ([L3.LineIdSafe3List])
        createDesign = do
          env <- ask
          let
            innerRadius = 1
            outerRadius = innerRadius + 1  
          safeVertexesInner <- HexR.runEitherRIO "safeVertexsInner" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.2)
             [( 90, innerRadius),
              (180, innerRadius),
              (270, innerRadius)
             ]
          innerLines <- runRIO env $ Pnt.toPoints safeVertexesInner >>= Line.toLines
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 90, outerRadius),
              (180, outerRadius),
              (270, outerRadius)
             ]
          outerLines <- runRIO env $ Pnt.toPoints safeVertexesOuter >>= Line.toLines
          return []
      
      env <- EnvLdr.loadTestEnvironment
      zippedLines <- runRIO env createDesign
      let
        toListsOfListOfIds = map (L3.evalSafeList3)  zippedLines
        toListOfListOfInts = map (map Env.evalLineId) toListsOfListOfIds
      assertEqual "zip together an inner and outer triangle" [[1,7,4,8],[2,8,5,9],[3,9,6,7]] toListOfListOfInts
      
   )
 _ <- runTestTT zipTriangleAlignedWithinTriangle


 let
  zipRectangleAlignedWithinRectangle = TestCase
   (do
      let
        createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ([L3.LineIdSafe3List])
        createDesign = do
          env <- ask
          let
            innerRadius = 1
            outerRadius = innerRadius + 1  
          safeVertexesInner <- HexR.runEitherRIO "safeVertexsInner" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.2)
             [( 60, innerRadius),
              (120, innerRadius),
              (240, innerRadius),
              (300, innerRadius)
             ]
          innerLines <- runRIO env $ Pnt.toPoints safeVertexesInner >>= Line.toLines
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 60, outerRadius),
              (120, outerRadius),
              (240, outerRadius),
              (300, outerRadius)
             ]
          outerLines <- runRIO env $ Pnt.toPoints safeVertexesOuter >>= Line.toLines
          return []
      
      env <- EnvLdr.loadTestEnvironment
      zippedLines <- runRIO env createDesign
      let
        toListsOfListOfIds = map (L3.evalSafeList3)  zippedLines
        toListOfListOfInts = map (map Env.evalLineId) toListsOfListOfIds
      assertEqual "zip together an inner and outer rectangle" [[1,9,5,10],[2,10,6,11],[3,11,7,12],[4,12,8,9]] toListOfListOfInts
      
   )
 _ <- runTestTT zipRectangleAlignedWithinRectangle
 
 return ()
