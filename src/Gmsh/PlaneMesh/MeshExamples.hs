{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{- |
Generate .geo file shapes that test out the functions in the Gmsh.PlaneMesh modules

Tests are in tests/PlaneMeshMeshTest.hs
-}
module Gmsh.PlaneMesh.MeshExamples(runTests, rectangleWithSingleHole, rectangleWithSingleRaisedHoleSmoothed, pentagonWithSingleRaisedRectangleSmoothed) where
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO
import qualified Gmsh.ToScript.BuiltIn as ScrB
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
import qualified Gmsh.CurveLoop as CL
import qualified Gmsh.PlaneSurface as PS
import qualified RIO.ByteString as B
import List.Base((>>+))
import qualified Gmsh.PlaneMesh.Mesh as Mesh

-- | Create a 2D rectangle with a hole in the center
rectangleWithSingleHole :: IO ()
rectangleWithSingleHole = do
  let
    innerRadius = 1
    innerVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.1)
           [( 60, innerRadius),
            (120, innerRadius),
            (240, innerRadius),
            (300, innerRadius)
           ]
    outerRadius = innerRadius + 1  
    outerVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [( 60, outerRadius),
            (120, outerRadius),
            (240, outerRadius),
            (300, outerRadius)
           ]
    createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
    createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC1
          safeVertexesInner <- HexR.runEitherRIO "safeVertexsInner" $ L3.toSafeList3 innerVertexes
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 outerVertexes 
          curveLoopInner <- runRIO env $ Pnt.toPoints safeVertexesInner >>= Line.toLines  >>= CL.toCurveLoop
          curveLoopOuter <- runRIO env $ Pnt.toPoints safeVertexesOuter >>= Line.toLines  >>= CL.toCurveLoop
          curveLoopsAdded <- HexR.runEitherRIO "curveLoopsAdded" (Right curveLoopOuter >>+ curveLoopInner) 
          _ <- runRIO env $ PS.toPlaneSurface curveLoopsAdded
          return ()
      
  env <- EnvLdr.loadEnvironment
  designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "lineZipTest_1"
  handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
  handleRef <- newIORef handle_
  runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign

-- | Rectangle with a raised inner rectangle. Uses Gmsh.ZipList to smooth the meshes.
-- Which is to say, join the 2 shapes by creating retangular surfaces between each of the lines.
rectangleWithSingleRaisedHoleSmoothed :: IO ()
rectangleWithSingleRaisedHoleSmoothed = do
  let
    createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
    createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC1
          let
            innerRadius = 1
            
            innerVertex =  Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.5)
                                            [( 60, innerRadius), (120, innerRadius), (240, innerRadius), (300, innerRadius)]
            outerRadius = innerRadius + 2               
            outerVertex = Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
                                            [( 60, outerRadius), (120, outerRadius), (240, outerRadius), (300, outerRadius)]
            
          _ <- runRIO env (Mesh.meshVertexToPoints
                             (Mesh.meshVertex $ Mesh.DoubleVertexMesh innerVertex outerVertex [])
                          )
                          >>= LZ.zipLines >>= LZ.zipCurves >>= LZ.zipPlanes
            
          return ()
      
  env <- EnvLdr.loadEnvironment
  designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "lineZipTest_2"
  handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
  handleRef <- newIORef handle_
  runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign
  
-- | rectangleWithSingleRaisedHoleSmoothed is missing 1 side so run some tests.
runTests :: IO ()
runTests = do
 let
  theseAreTheInitalInnerAndOuterVertexes = TestCase $ assertEqual
   "The inner and outer rectangle vectors before meshing" 
   (Mesh.DoubleVertexMesh [V.newVertex 0.5 0.87 0.5, V.newVertex (-0.5) 0.87 0.5, V.newVertex (-0.5) (-0.87) 0.5 ,V.newVertex 0.5 (-0.87) 0.5] --inner hole
                          [V.newVertex 1.5 2.6 0.0, V.newVertex (-1.5) 2.6 0.0, V.newVertex (-1.5) (-2.6) 0.0, V.newVertex 1.5 (-2.6) 0.0]     --outer rectangle
                          []
   )
   (let
      innerRadius = 1
      innerVertex =  Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.5)
                                            [( 60, innerRadius), (120, innerRadius), (240, innerRadius), (300, innerRadius)]
      outerRadius = innerRadius + 2               
      outerVertex = Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
                                            [( 60, outerRadius), (120, outerRadius), (240, outerRadius), (300, outerRadius)]
      zipVertexResults = Mesh.DoubleVertexMesh innerVertex outerVertex []
    in
       zipVertexResults
       
   )
 _ <- runTestTT theseAreTheInitalInnerAndOuterVertexes

 let
  nowLookAtTheMeshedVertexes = TestCase $ assertEqual
   "The inner and outer rectangle vectors after meshing" 
   (Mesh.DoubleVertexMesh 
     [] [] $ reverse
     [
       [V.newVertex 0.5 (-0.87) 0.5, V.newVertex 1.5 (-2.6) 0.0, V.newVertex 1.5 2.6 0.0, V.newVertex 0.5 0.87 0.5],
       [V.newVertex (-0.5) (-0.87) 0.5, V.newVertex (-1.5) (-2.6) 0.0, V.newVertex 1.5 (-2.6) 0.0, V.newVertex 0.5 (-0.87) 0.5],
       [V.newVertex (-0.5) 0.87 0.5, V.newVertex (-1.5) 2.6 0.0, V.newVertex (-1.5) (-2.6) 0.0, V.newVertex (-0.5) (-0.87) 0.5],
       [V.newVertex 0.5 0.87 0.5, V.newVertex 1.5 2.6 0.0, V.newVertex (-1.5) 2.6 0.0, V.newVertex (-0.5) 0.87 0.5]
     ]
   )
   (let
      innerRadius = 1
      innerVertex =  Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.5)
                                            [( 60, innerRadius), (120, innerRadius), (240, innerRadius), (300, innerRadius)]
      outerRadius = innerRadius + 2               
      outerVertex = Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
                                            [( 60, outerRadius), (120, outerRadius), (240, outerRadius), (300, outerRadius)]
      beforeMeshingResults = Mesh.DoubleVertexMesh innerVertex outerVertex []
      afterMeshingResults  = Mesh.meshVertex beforeMeshingResults
    in
      afterMeshingResults
       
   )
 _ <- runTestTT nowLookAtTheMeshedVertexes


 
 return ()

 
-- | Rectangle with a raised inner rectangle. Uses Gmsh.ZipList to smooth the meshes.
-- Which is to say, join the 2 shapes by creating retangular surfaces between each of the lines.
pentagonWithSingleRaisedRectangleSmoothed :: IO ()
pentagonWithSingleRaisedRectangleSmoothed = do
  let
    createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
    createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC1
          let
            innerRadius = 1
            
            innerVertex =  Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.5)
                                            [( 60, innerRadius), (120, innerRadius), (240, innerRadius), (300, innerRadius)]
            outerRadius = innerRadius + 2               
            outerVertex = Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
                                            [( 60, outerRadius), (120, outerRadius), (240, outerRadius), (300, outerRadius), (330, innerRadius + 1)]
            
          _ <- runRIO env (Mesh.meshVertexToPoints
                             (Mesh.meshVertex $ Mesh.DoubleVertexMesh innerVertex outerVertex [])
                          )
                          >>= LZ.zipLines >>= LZ.zipCurves >>= LZ.zipPlanes
            
          return ()
      
  env <- EnvLdr.loadEnvironment
  designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "lineZipTest_2"
  handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
  handleRef <- newIORef handle_
  runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign
