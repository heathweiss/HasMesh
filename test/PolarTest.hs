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
--import qualified Gmsh.Point as Pnt
import qualified Utils.RunExceptions as HexR
import qualified Utils.List as L
import qualified Gmsh.Gmsh as Gmsh

runTests = do
 P.putStrLn $ "=============== Polar Test ====================="  


 let
  createNewPolarCoordinate = TestCase $ assertEqual
   "create a 135 degree polar cood in the 4th quadrant"
   (Polar.newPolarCoordinate (Polar.Degree 135) (Polar.Radius $ sqrt 2) (Axis.ZAxis 10))
   (Polar.newPolarCoordinate (Polar.Degree 135) (Polar.Radius $ sqrt 2) (Axis.ZAxis 10)
   ) 
 runTestTT createNewPolarCoordinate

--------------------------------------------------------------------------------------------------
 -- PolarCoordinates: Create a vertex based on radius == 1 and various angles.
 --------------------------------------------------------------------------------------------------
 let
  createAQuadrant1VertexFromAPolarCoordinate = TestCase $ assertEqual
   "create a 45 degree vertex in quadrant 1, from polar cood"
   (V.newVertex 1 1 0)
   (Polar.toVertex $ Polar.newPolarCoordinate (Polar.Degree 45) (Polar.Radius (sqrt 2)) (Axis.ZAxis 0)
   ) 
 runTestTT createAQuadrant1VertexFromAPolarCoordinate


 let
  createAQuadrant2VertexFromAPolarCoordinate = TestCase $ assertEqual
   "create a 135 degree vertex in quadrant 2, from polar cood"
   (V.newVertex (-1.0) (1) 10)
   (Polar.toVertex $ Polar.newPolarCoordinate (Polar.Degree 135) (Polar.Radius (sqrt 2)) (Axis.ZAxis 10)
   ) 
 runTestTT createAQuadrant2VertexFromAPolarCoordinate


 let
  createAQuadrant3VertexFromAPolarCoordinate = TestCase $ assertEqual
   "create a 215 degree vertex in the 3rd quadrant, from polar cood"
   (V.newVertex (-1.0) (-1) 10)
   (Polar.toVertex $ Polar.newPolarCoordinate (Polar.Degree 225) (Polar.Radius (sqrt 2)) (Axis.ZAxis 10)
   ) 
 runTestTT createAQuadrant3VertexFromAPolarCoordinate


 let
  createAQuadrant4VertexFromAPolarCoordinate = TestCase $ assertEqual
   "create a 315 degree vertex in the 4th quadrant, from polar cood"
   (V.newVertex (1) (-1.0) 10)
   (Polar.toVertex $ Polar.newPolarCoordinate (Polar.Degree 315) (Polar.Radius (sqrt 2)) (Axis.ZAxis 10)
   ) 
 runTestTT createAQuadrant4VertexFromAPolarCoordinate

 ----------------------------------------------------------------------------------------------------
 -- PolarCoordinates: create Vertex from a tuple
 ----------------------------------------------------------------------------------------------------
 let
  createVertexFromSingleTuple = TestCase $ assertEqual
   "create a 45 degree vertex in the 4th quadrant, from tuple"
   (V.newVertex (1) (1.0) 0)
   (Polar.toVertex $ Polar.newPolarCoordinateFromTuple (45, sqrt 2, 0)
   ) 
 runTestTT createVertexFromSingleTuple

 let
  createVertexFromSingleTuple2 = TestCase $ assertEqual
   "create a 135 degree vertex in the 4th quadrant, from tuple"
   (V.newVertex (-1) (1.0) 10)
   (Polar.toVertex $ Polar.newPolarCoordinateFromTuple (135, sqrt 2, 10)
   ) 
 runTestTT createVertexFromSingleTuple2


 let
  createMultVertexFromListOfTuple = TestCase $ assertEqual
   "create a 315 degree vertex in the 4th quadrant, from tuple"
   [V.newVertex (1)    (1.0) 0,
    V.newVertex (-1.0) (1)   10
   ]
   (  Polar.newVertexFromPolarCoordinatesTuples
      [(45, sqrt 2, 0),
       (135,sqrt 2,10)
      ]
   ) 
 runTestTT createMultVertexFromListOfTuple




-- --------------------------------------------------------------------------------------------
-- Tests to make TutorialT1 work as it is not putting # 3 and 4 points .
-- See additional tests in PointTest.
 let
  createThe4VetexT1 = TestCase $ assertEqual
   "create all 4 vertex for the Tutorial T1_linesFromPolarTuples example "
   [V.newVertex 25.0 43.3 0.0,
    V.newVertex  (-25.0) 43.3 0.0,
    V.newVertex (-25.0) (-43.3) 0.0,
    V.newVertex 25.0 (-43.3) 0.0
   ]
   (  let
        radius = 50
        height = 0
      in
        Polar.newVertexFromPolarCoordinatesTuples
              [(60, radius, height),
               (120, radius, height),
               (240, radius, height),
               (300, radius, height)
              ]
   )
 runTestTT createThe4VetexT1

 let
  createThe4PointsT1 = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let
        radius = 50
        height = 0
        vertexs =
          Polar.newVertexFromPolarCoordinatesTuples
              [(60, radius, height),
               (120, radius, height),
               (240, radius, height),
               (300, radius, height)
              ]
      points <- runRIO env $ Gmsh.toPoints vertexs >>= HexR.runEitherRIO "points"
      assertEqual
       "All i get is 1 2 1 2. Need to cx the hashes."
       (L.Cons (Gmsh.PointId $ Gmsh.PointInt 1) (Gmsh.PointId $ Gmsh.PointInt 2) (Gmsh.PointId $ Gmsh.PointInt 3)  [(Gmsh.PointId $ Gmsh.PointInt 4)]  L.Nil)
       (points )
   )
 runTestTT createThe4PointsT1

 let
   lookAtVectorHashes = TestCase $ assertEqual
     "hashes are now unique now that is uses 'show' for hashing. This fixed problem of not all vectors creating points"
      [6029490868410831343,-8083631980746447389,2308339117287999229,830237734104123193]
      (let
        radius = 50
        height = 0
        vertexs =
          Polar.newVertexFromPolarCoordinatesTuples
              [(60, radius, height),
               (120, radius, height),
               (240, radius, height),
               (300, radius, height)
              ]
       in
        map H.hash vertexs
      )
 runTestTT lookAtVectorHashes
