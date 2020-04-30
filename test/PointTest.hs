{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PointTest(runTests) where
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO

import qualified Geometry.Vertex as V
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Point as Pnt  
import qualified Utils.Environment as Env
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR
--import qualified Gmsh.ID as ID
import qualified Utils.Environment as Env
import qualified Utils.EnvironmentLoader as EnvLdr 


runTests = do
 P.putStrLn $ "=============== PointTest ====================="

------------------------------------------------------------------------------------------------
----------------------------- Eq -------------------------
 let
  pointsAreEqual = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      point111 <- runRIO env $ Pnt.toPoint $ Geo.newVertex  1 1 1
      pointFromSameVector <- runRIO env $ Pnt.toPoint $ Geo.newVertex  1 1 1
      assertEqual "points are Eq" True (point111 == pointFromSameVector)
   )
 runTestTT pointsAreEqual

 let
  pointsAreEqualFromEnviro = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      point111 <- runRIO env $ Pnt.toPoint $ Geo.newVertex  1 1 1
      pointFromSameVector <- runRIO env $ Pnt.toPoint $ Geo.newVertex  1 1 1
      assertEqual "create a point from the Environment module, and test for ==" True (point111 == pointFromSameVector)
   )
 runTestTT pointsAreEqualFromEnviro
  

 

 let
  pointsAreNotEqual = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      point111 <- runRIO env $ Pnt.toPoint $ Geo.newVertex  1 1 1
      point222 <- runRIO env $ Pnt.toPoint $ Geo.newVertex  2 2 2
      assertEqual "points are not Eq" True  (point222 /= point111)
   )
 runTestTT pointsAreNotEqual

 let
  safe3ListsFromSameVectorsAreEq = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs1 = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      points1 <- runRIO env $ Pnt.toPoints vertexs1 >>= HexR.runEitherRIO "points"
      points2 <- runRIO env $ Pnt.toPoints vertexs1 >>= HexR.runEitherRIO "points"
      assertEqual
       "2 safe lists created from same vertex are eq"
       points1 points2
   )  
 _ <- runTestTT  safe3ListsFromSameVectorsAreEq

 let
  safe3ListsFromDiffVectorsAreNotEq = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs1 = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
        vertexs2 = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  33 33 33]
      points1 <- runRIO env $ Pnt.toPoints vertexs1 >>= HexR.runEitherRIO "points"
      points2 <- runRIO env $ Pnt.toPoints vertexs2 >>= HexR.runEitherRIO "points"
      assertEqual
       "2 safe lists created from different vertex are not Eq"
       False (points1 ==  points2)
   )
 _ <- runTestTT safe3ListsFromDiffVectorsAreNotEq
 

 
-------------------------------------------------------------------------------------------------
------------ create points from [vertex] with length 2 - 5 ----------------------------------


--Pnt.toPoints will return an exception if the length [vertex] < 3
--toDo: create a PointsTest and move this there. Then create tests for [0 vertex] [1 vertex]
 let
  toPointsFailsWith2Vertex = TestCase
   (do
      
      
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6]
      points <- runRIO env $ Pnt.toPoints vertexs
      assertEqual "get the vector id from an ioref" (Left (Hex.SafeList3MinError "length == 2")) points -- result 
   )
 _ <- runTestTT toPointsFailsWith2Vertex


 let
  create3PointsFrom3Vertex = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6, Geo.newVertex  7 8 9]
      eitherPoints <- runRIO env $ Pnt.toPoints vertexs
      assertEqual "get the vector id from an ioref"
       (Right [1, 2, 3])
       (case eitherPoints of
          Right points -> Right $ map Env.evalPointId (L.evalSafeList3 points)
          Left err -> Left err
       )
   )
 _ <- runTestTT create3PointsFrom3Vertex
 

  
 let
  create4PointsFrom4Vertex = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs = [Geo.newVertex 1 1 1, Geo.newVertex 2 2 2, Geo.newVertex 3 3 3, Geo.newVertex 4 4 4]
      eitherPoints <- runRIO env $ Pnt.toPoints vertexs
      assertEqual "get the vector id from an ioref"
       (Right [1,2,3,4])
       (case eitherPoints of
          Right points -> Right $  map Env.evalPointId (L.evalSafeList3 points)
          Left err -> Left err
          
       )
   )
 _ <- runTestTT create4PointsFrom4Vertex


 ------------------------------------------ test for isOpen: so last point != head point--------------------------------------------------

 let
  create3PointsFrom3ClosedVertex = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  1 1 1]
      eitherPoints <- runRIO env $ Pnt.toPoints vertexs
      assertEqual "create points from closed vectors lenth == 3"
       (Left $ Hex.PointIdSafe3ListIsClosed "PointIdList is closed")
       (case eitherPoints of
          Right points -> Right $ L.evalSafeList3 points
          Left err -> Left err
       )
   )
 runTestTT create3PointsFrom3ClosedVertex
 


{-
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- internal testing ------------------------------------------------
-- requires export of Gmsh.Point.toPoints'

-------------------------------------- toPoints' ----------------------------------------
-- Use the toPoints' fx in the same way that toPoints uses it.
 let
  getSafePointIdListFor3Vectors = TestCase
   (do
      env <- Env.loadTestEnvironment
      xPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 1 1 1 -- initialVertex
      yPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 2 2 2 --y
      zPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 3 3 3 --z
      initialSafeList <- HexR.runEitherIO "initialSafeList" (L.toSafeList3 [xPointId, yPointId, zPointId]) 
      let
        initialVertex = Geo.newVertex  1 1 1
        vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
        initialTailVertexs = []
      --                                         used to see if safelist is closed when done
      --                                                       empty as 1st 3 were used for safelist
      --                                                                          toPoints reverses working list as per standard working list system for appending till final iteration, then reversing
      eitherPoints <- runRIO env $ Pnt.toPoints' initialVertex initialTailVertexs (L.reverseSafeList3 initialSafeList )
      assertEqual
       "create a PointIdSafe3List from 3 vectors"
       (Right [ID.PointId (ID.PointInt 1),ID.PointId (ID.PointInt 2),ID.PointId (ID.PointInt 3)])
       (case eitherPoints of
          Right rightPoints -> Right $ L.evalSafeList3 rightPoints
          Left ex -> Left $ show ex
       )
   )
 runTestTT getSafePointIdListFor3Vectors

 

 let
  getSafePointIdListFor4Vectors = TestCase
   (do
      env <- Env.loadTestEnvironment
      xPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 1 1 1 -- initialVertex
      yPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 2 2 2 --y
      zPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 3 3 3 --z
      --z'PointId <- runRIO env $ Pnt.toPoint $ V.newVertex 4 4 4 --z':zs
      initialSafeList <- HexR.runEitherIO "initialSafeList" (L.toSafeList3 [xPointId, yPointId, zPointId]) 
      let
        initialVertex = Geo.newVertex  1 1 1
        vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, V.newVertex 4 4 4]
        initialTailVertexs = [V.newVertex 4 4 4]
      --                                         used to see if safelist is closed when done
      --                                                       empty as 1st 3 were used for safelist
      --                                                                          toPoints reverses working list as per standard working list system for appending till final iteration, then reversing
      eitherPoints <- runRIO env $ Pnt.toPoints' initialVertex initialTailVertexs (L.reverseSafeList3 initialSafeList )
      assertEqual
       "create a PointIdSafe3List from 4 vectors"
       (Right [ID.PointId (ID.PointInt 1),ID.PointId (ID.PointInt 2),ID.PointId (ID.PointInt 3), ID.PointId (ID.PointInt 4)])
       (case eitherPoints of
          Right rightPoints -> Right $ L.evalSafeList3 rightPoints
          Left ex -> Left $ show ex
       )
   )
 runTestTT getSafePointIdListFor4Vectors


 let
  getSafePointIdListFor5Vectors = TestCase
   (do
      env <- Env.loadTestEnvironment
      xPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 1 1 1 -- initialVertex
      yPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 2 2 2 --y
      zPointId <- runRIO env $ Pnt.toPoint $ V.newVertex 3 3 3 --z
      initialSafeList <- HexR.runEitherIO "initialSafeList" (L.toSafeList3 [xPointId, yPointId, zPointId]) 
      let
        initialVertex = Geo.newVertex  1 1 1
        vertexs = [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, V.newVertex 4 4 4, V.newVertex 5 5 5]
        initialTailVertexs = [V.newVertex 4 4 4, V.newVertex 5 5 5]
      --                                         used to see if safelist is closed when done
      --                                                       empty as 1st 3 were used for safelist
      --                                                                          toPoints reverses working list as per standard working list system for appending till final iteration, then reversing
      eitherPoints <- runRIO env $ Pnt.toPoints' initialVertex initialTailVertexs (L.reverseSafeList3 initialSafeList )
      assertEqual
       "create a PointIdSafe3List from 5 vectors"
       (Right [ID.PointId (ID.PointInt 1),ID.PointId (ID.PointInt 2),ID.PointId (ID.PointInt 3), ID.PointId (ID.PointInt 4), ID.PointId (ID.PointInt 5)])
       (case eitherPoints of
          Right rightPoints -> Right $ L.evalSafeList3 rightPoints
          Left ex -> Left $ show ex
       )
   )
 runTestTT getSafePointIdListFor5Vectors
-}

