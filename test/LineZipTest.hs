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
import qualified List.Safe1 as L1
import qualified List.Safe3 as L3
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import qualified Gmsh.PlaneMesh.LineZip as LZ
import qualified Geometry.Axis as Axis
import qualified Geometry.Polar as Polar

runTests = do
 P.putStrLn $ "=============== Line Zip Test ====================="

--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- create points from vertexes ---------------------------------------------------------------------
 let
  zipToFromPointsTriangleAlignedWithinTriangle = TestCase
   (do
      let
        createZippedPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ([L3.PointIdSafe3List])
        createZippedPoints = do
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
          innerPoints <- runRIO env $ Pnt.toPoints safeVertexesInner 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 90, outerRadius),
              (180, outerRadius),
              (270, outerRadius)
             ]
          outerPoints <- runRIO env $ Pnt.toPoints safeVertexesOuter
          LZ.zipPoints innerPoints outerPoints
          
      env <- EnvLdr.loadTestEnvironment
      zippedPoints <- runRIO env createZippedPoints
      assertEqual "triangles: zip together an inner [points] and outer [points] to a zipped [[points]]" [[1,4,5,2],[2,5,6,3],[3,6,4,1]]
                      (map (map Env.evalPointId)
                           (map L3.evalSafeList3  zippedPoints)
                      )
      
   )
 _ <- runTestTT zipToFromPointsTriangleAlignedWithinTriangle

 let
  zipToFromPointsRectangleAlignedWithinRectangle = TestCase
   (do
      let
        createZippedPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ([L3.PointIdSafe3List])
        createZippedPoints = do
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
          innerPoints <- runRIO env $ Pnt.toPoints safeVertexesInner 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 60, outerRadius),
              (120, outerRadius),
              (240, outerRadius),
              (300, outerRadius)
             ]
          outerPoints <- runRIO env $ Pnt.toPoints safeVertexesOuter
          LZ.zipPoints innerPoints outerPoints
      
      env <- EnvLdr.loadTestEnvironment
      zippedPoints <- runRIO env createZippedPoints
      assertEqual "rectangles: zip together an inner [points] and outer [points] to a zipped [[points]]"
                      [[1,5,6,2],[2,6,7,3],[3,7,8,4],[4,8,5,1]]
                      (map (map Env.evalPointId)
                           (map L3.evalSafeList3  zippedPoints)
                      )
      
   )
 _ <- runTestTT zipToFromPointsRectangleAlignedWithinRectangle

 let
  zipToFromPointsPentagonAlignedWithinPentagon = TestCase
   (do
      let
        createZippedPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ([L3.PointIdSafe3List])
        createZippedPoints = do
          env <- ask
          let
            innerRadius = 1
            outerRadius = innerRadius + 1  
          safeVertexesInner <- HexR.runEitherRIO "safeVertexsInner" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.2)
             [( 0, innerRadius),
              (72, innerRadius),
              (144, innerRadius),
              (216, innerRadius),
              (288, innerRadius)
             ]
          innerPoints <- runRIO env $ Pnt.toPoints safeVertexesInner 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 0, outerRadius),
              (72, outerRadius),
              (144, outerRadius),
              (216, outerRadius),
              (288, outerRadius)
             ]
          outerPoints <- runRIO env $ Pnt.toPoints safeVertexesOuter
          LZ.zipPoints innerPoints outerPoints
      
      env <- EnvLdr.loadTestEnvironment
      zippedPoints <- runRIO env createZippedPoints
      assertEqual "pentagon: zip together an inner [points] and outer [points] to a zipped [[points]]"
                      [[1,6,7,2],[2,7,8,3],[3,8,9,4],[4,9,10,5],[5,10,6,1]]
                      (map (map Env.evalPointId)
                           (map L3.evalSafeList3  zippedPoints)
                      )
      
   )
 _ <- runTestTT zipToFromPointsPentagonAlignedWithinPentagon

 let
  zipToFromPointsHexagonAlignedWithinHexagon = TestCase
   (do
      let
        createZippedPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ([L3.PointIdSafe3List])
        createZippedPoints = do
          env <- ask
          let
            innerRadius = 1
            outerRadius = innerRadius + 1  
          safeVertexesInner <- HexR.runEitherRIO "safeVertexsInner" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.2)
             [( 0, innerRadius),
              (60, innerRadius),
              (120, innerRadius),
              (180, innerRadius),
              (240, innerRadius),
              (300, innerRadius)
             ]
          innerPoints <- runRIO env $ Pnt.toPoints safeVertexesInner 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 0, outerRadius),
              (60, outerRadius),
              (120, outerRadius),
              (180, outerRadius),
              (240, outerRadius),
              (300, outerRadius)
             ]
          outerPoints <- runRIO env $ Pnt.toPoints safeVertexesOuter
          LZ.zipPoints innerPoints outerPoints
      
      env <- EnvLdr.loadTestEnvironment
      zippedPoints <- runRIO env createZippedPoints
      assertEqual "Hexagon: zip together an inner [points] and outer [points] to a zipped [[points]]"
                      [[1,7,8,2],[2,8,9,3],[3,9,10,4],[4,10,11,5],[5,11,12,6],[6,12,7,1]]
                      (map (map Env.evalPointId)
                           (map L3.evalSafeList3  zippedPoints)
                      )
      
   )
 _ <- runTestTT zipToFromPointsHexagonAlignedWithinHexagon

 let
  zipToFromPointsDecagonAlignedWithinDecagon = TestCase
   (do
      let
        createZippedPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ([L3.PointIdSafe3List])
        createZippedPoints = do
          env <- ask
          let
            innerRadius = 1
            outerRadius = innerRadius + 1  
          safeVertexesInner <- HexR.runEitherRIO "safeVertexsInner" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.2)
             [( 0, innerRadius),
              (36, innerRadius),
              (72, innerRadius),
              (108, innerRadius),
              (144, innerRadius),
              (180, innerRadius),
              (216, innerRadius),
              (252, innerRadius),
              (288, innerRadius),
              (324, innerRadius)
             ]
          innerPoints <- runRIO env $ Pnt.toPoints safeVertexesInner 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 0, outerRadius),
              (36, outerRadius),
              (72, outerRadius),
              (108, outerRadius),
              (144, outerRadius),
              (180, outerRadius),
              (216, outerRadius),
              (252, outerRadius),
              (288, outerRadius),
              (324, outerRadius)
             ]
          outerPoints <- runRIO env $ Pnt.toPoints safeVertexesOuter
          LZ.zipPoints innerPoints outerPoints
      
      env <- EnvLdr.loadTestEnvironment
      zippedPoints <- runRIO env createZippedPoints
      assertEqual "Decagon: zip together an inner [points] and outer [points] to a zipped [[points]]"
                      [[1,11,12,2],[2,12,13,3],[3,13,14,4],[4,14,15,5],[5,15,16,6],[6,16,17,7],
                       [7,17,18,8],[8,18,19,9],[9,19,20,10],[10,20,11,1]
                      ]
                      (map (map Env.evalPointId)
                           (map L3.evalSafeList3  zippedPoints)
                      )
      
   )
 _ <- runTestTT zipToFromPointsDecagonAlignedWithinDecagon

--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------- create lines from points -----------------------------------------------------------------

 let
  zipToLines_TriangleAlignedWithinTriangle = TestCase
   (do
      let
        createZippedLines :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env [L3.LineIdSafe3List]
        createZippedLines = do
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
          innerPoints <- runRIO env $ Pnt.toPoints safeVertexesInner 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 90, outerRadius),
              (180, outerRadius),
              (270, outerRadius)
             ]
          outerPoints <- runRIO env $ Pnt.toPoints safeVertexesOuter
          runRIO env( LZ.zipPoints innerPoints outerPoints) >>= LZ.zipLines
          
      
      env <- EnvLdr.loadTestEnvironment
      zippedLines <- runRIO env createZippedLines
      
      assertEqual "triangles: zip together an inner [points] and outer [points] to a zipped [[lines]]" [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
                      (map (map Env.evalLineId)
                           (map L3.evalSafeList3  zippedLines)
                      )
   )
 _ <- runTestTT zipToLines_TriangleAlignedWithinTriangle


--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------- create curves from lines -----------------------------------------------------------------

 let
  zipToCurves_TriangleAlignedWithinTriangle = TestCase
   (do
      let
        createZippedLines :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env [L1.CurveIdSafe1List]
        createZippedLines = do
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
          innerPoints <- runRIO env $ Pnt.toPoints safeVertexesInner 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 90, outerRadius),
              (180, outerRadius),
              (270, outerRadius)
             ]
          outerPoints <- runRIO env $ Pnt.toPoints safeVertexesOuter

          --(runRIO env $ LZ.zipLines $ LZ.zipPoints innerPoints outerPoints) >>= LZ.zipCurves
          runRIO env (LZ.zipPoints innerPoints outerPoints) >>= LZ.zipLines >>= LZ.zipCurves
      
      env <- EnvLdr.loadTestEnvironment
      zippedCurves <- runRIO env createZippedLines
      
      assertEqual "triangles: zip together an inner [points] and outer [points] to a zipped [[curve loops]]" [[1],[2],[3]]
                      (map (map Env.evalCurveLoopId)
                           (map L1.evalSafeList1  zippedCurves)
                      )
   )
 _ <- runTestTT zipToCurves_TriangleAlignedWithinTriangle


--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------- create plane surfaces from  curves -----------------------------------------------------------

 let
  zipToPlanes_TriangleAlignedWithinTriangle = TestCase
   (do
      let
        createZippedLines :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env [L1.PlaneSurfaceSafe1List]
        createZippedLines = do
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
          innerPoints <- runRIO env $ Pnt.toPoints safeVertexesInner 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 $
            Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
             [( 90, outerRadius),
              (180, outerRadius),
              (270, outerRadius)
             ]
          outerPoints <- runRIO env $ Pnt.toPoints safeVertexesOuter

          --(runRIO env $ LZ.zipLines $ LZ.zipPoints innerPoints outerPoints) >>= LZ.zipCurves >>= LZ.zipPlanes
          runRIO env (LZ.zipPoints innerPoints outerPoints) >>= LZ.zipLines >>= LZ.zipCurves >>= LZ.zipPlanes
          
      
      env <- EnvLdr.loadTestEnvironment
      zippedPlanes <- runRIO env createZippedLines
      
      assertEqual "triangles: zip together an inner [points] and outer [points] to a zipped [[curve loops]]" [[1],[2],[3]]
                      (map (map Env.evalPlaneSurfaceId)
                           (map L1.evalSafeList1  zippedPlanes)
                      )
   )
 _ <- runTestTT zipToPlanes_TriangleAlignedWithinTriangle



 --return () from runTests 
 return ()
