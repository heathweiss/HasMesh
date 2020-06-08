{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PlaneMeshMeshTest(runTests) where
  
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
import qualified Gmsh.Line as Line
import qualified Gmsh.Point as Pnt
import qualified List.Safe1 as L1
import qualified List.Safe3 as L3
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import qualified Gmsh.PlaneMesh.LineZip as LZ
import qualified Geometry.Axis as Axis
import qualified Geometry.Polar as Polar
import qualified Gmsh.PlaneMesh.Mesh as Mesh
runTests = do
 P.putStrLn "=============== PlaneMesh Mesh Tests ====================="  

-----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------
-- goes with Gmsh.PlaneMesh.MeshExamples.rectangleWithSingleRaisedHoleSmoothed
-- The .geo file can be gen'd with above fx. 
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
     [] []
     [
       [V.newVertex 0.5 0.87 0.5, V.newVertex 1.5 2.6 0.0, V.newVertex (-1.5) 2.6 0.0, V.newVertex (-0.5) 0.87 0.5],
       [V.newVertex (-0.5) 0.87 0.5, V.newVertex (-1.5) 2.6 0.0, V.newVertex (-1.5) (-2.6) 0.0, V.newVertex (-0.5) (-0.87) 0.5],
       [V.newVertex (-0.5) (-0.87) 0.5, V.newVertex (-1.5) (-2.6) 0.0, V.newVertex 1.5 (-2.6) 0.0, V.newVertex 0.5 (-0.87) 0.5],
       [V.newVertex 0.5 (-0.87) 0.5, V.newVertex 1.5 (-2.6) 0.0, V.newVertex 1.5 2.6 0.0, V.newVertex 0.5 0.87 0.5]
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

