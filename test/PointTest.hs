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
      safeVertex <- HexR.runEitherIO "safeVertex" $ L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      points1 <- runRIO env $ Pnt.toPoints safeVertex
      points2 <- runRIO env $ Pnt.toPoints safeVertex
      assertEqual
       "2 safe lists created from same vertex are eq"
       points1 points2
   )  
 _ <- runTestTT  safe3ListsFromSameVectorsAreEq

 let
  safe3ListsFromDiffVectorsAreNotEq = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs1 <- HexR.runEitherIO "safeVertex1" $ L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]
      safeVertexs2 <- HexR.runEitherIO "safeVertex2" $ L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  33 33 33]
      points1 <- runRIO env $ Pnt.toPoints safeVertexs1 
      points2 <- runRIO env $ Pnt.toPoints safeVertexs2
      assertEqual
       "2 safe lists created from different vertex are not Eq"
       False (points1 ==  points2)
   )
 _ <- runTestTT safe3ListsFromDiffVectorsAreNotEq
 

 
-------------------------------------------------------------------------------------------------
------------ create points from [vertex] with length 2 - 5 ----------------------------------

 let
  create3PointsFrom3Vertex = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs <- HexR.runEitherIO "safeVertex" $ L.toSafeList3 [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6, Geo.newVertex  7 8 9]
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "create points from safeList3 vertex length == 3"
       [1, 2, 3]
       (map Env.evalPointId (L.evalSafeList3 points))
   )
 _ <- runTestTT create3PointsFrom3Vertex
 

  
 let
  create4PointsFrom4Vertex = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs <- HexR.runEitherIO "safeVertex" $ L.toSafeList3 [Geo.newVertex 1 1 1, Geo.newVertex 2 2 2, Geo.newVertex 3 3 3, Geo.newVertex 4 4 4]
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "get the vector id from an ioref"
        [1,2,3,4]
        (map Env.evalPointId (L.evalSafeList3 points))
   )
 _ <- runTestTT create4PointsFrom4Vertex

---------------------------------------- create a [points] from a safe [vertex]-----------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

 let
  unique3VertexToPoints = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs  <- HexR.runEitherIO "fjjf" $ ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3]):: Either Hex.HasMeshException L.VertexSafe3List)
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "create points from vectors safelist length == 3"
       [1,2,3]
       (map (Env.evalPointId) $ L.evalSafeList3 points)
   )
 runTestTT unique3VertexToPoints

 let
  nonUnique3VertexToPointsFails = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let 
        safeVertexs = ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  2 2 2]):: Either Hex.HasMeshException L.VertexSafe3List)
      
      assertEqual "create points from vectors safelist fails for non unique vertex"
       (Left(Hex.NonUniqueVertex "non unique safe [Vertex]"))
       (case safeVertexs of
          Right vertexs -> Right $ L.evalSafeList3 vertexs
          Left (Hex.NonUniqueVertex msg) -> Left (Hex.NonUniqueVertex msg)
       )
   )
 runTestTT nonUnique3VertexToPointsFails
 
 let
  unique4VertexToPoints = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs  <- HexR.runEitherIO "safe vertex" $ ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4]):: Either Hex.HasMeshException L.VertexSafe3List)
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "create points from vectors safelist length == 3"
       [1,2,3,4]
       (map (Env.evalPointId) $ L.evalSafeList3 points)
   )
 runTestTT unique4VertexToPoints

 let
  unique6VertexToPoints = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs  <- HexR.runEitherIO "safe vertex" $ ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2,  Geo.newVertex  3 3 3, 
                                                                        Geo.newVertex  11 1 1, Geo.newVertex 12 2 2, Geo.newVertex  13 3 3
                                                                       ]):: Either Hex.HasMeshException L.VertexSafe3List)
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "create points from vectors safelist length == 6"
       [1,2,3,4,5,6]
       (map (Env.evalPointId) $ L.evalSafeList3 points)
   )
 runTestTT unique6VertexToPoints

 let
  unique7VertexToPoints = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs  <- HexR.runEitherIO "safe vertex" $ ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2,  Geo.newVertex  3 3 3, 
                                                                        Geo.newVertex  11 1 1, Geo.newVertex 12 2 2, Geo.newVertex  13 3 3,
                                                                        Geo.newVertex  21 1 1
                                                                       ]):: Either Hex.HasMeshException L.VertexSafe3List)
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "create points from vectors safelist length == 7"
       [1,2,3,4,5,6,7]
       (map (Env.evalPointId) $ L.evalSafeList3 points)
   )
 runTestTT unique7VertexToPoints

 let
  unique8VertexToPoints = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs  <- HexR.runEitherIO "safe vertex" $ ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2,  Geo.newVertex  3 3 3, 
                                                                        Geo.newVertex  11 1 1, Geo.newVertex 12 2 2, Geo.newVertex  13 3 3,
                                                                        Geo.newVertex  21 1 1, Geo.newVertex 22 2 2
                                                                       ]):: Either Hex.HasMeshException L.VertexSafe3List)
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "create points from vectors safelist length == 8"
       [1,2,3,4,5,6,7,8]
       (map (Env.evalPointId) $ L.evalSafeList3 points)
   )
 runTestTT unique8VertexToPoints

 let
  unique9VertexToPoints = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs  <- HexR.runEitherIO "safe vertex" $ ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2,  Geo.newVertex  3 3 3, 
                                                                        Geo.newVertex  11 1 1, Geo.newVertex 12 2 2, Geo.newVertex  13 3 3,
                                                                        Geo.newVertex  21 1 1, Geo.newVertex  22 2 2, Geo.newVertex  23 3 3
                                                                       ]):: Either Hex.HasMeshException L.VertexSafe3List)
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "create points from vectors safelist length == 9"
       [1,2,3,4,5,6,7,8,9]
       (map (Env.evalPointId) $ L.evalSafeList3 points)
   )
 runTestTT unique9VertexToPoints

 

 let
  unique12VertexToPoints = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      safeVertexs  <- HexR.runEitherIO "safe vertex" $ ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  3 3 3, Geo.newVertex 4 4 4,
                                                                       Geo.newVertex  11 1 1, Geo.newVertex  12 2 2, Geo.newVertex  13 3 3, Geo.newVertex 14 4 4,
                                                                       Geo.newVertex  21 1 1, Geo.newVertex  22 2 2, Geo.newVertex  23 3 3, Geo.newVertex 24 4 4
                                                                       ]):: Either Hex.HasMeshException L.VertexSafe3List)
      points <- runRIO env $ Pnt.toPoints safeVertexs
      assertEqual "create points from vectors safelist length == 12"
       [1,2,3,4,5,6,7,8,9,10,11,12]
       (map (Env.evalPointId) $ L.evalSafeList3 points)
   )
 runTestTT unique12VertexToPoints

 
 let
  nonUnique4VertexToPointsFails = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let 
        safeVertexs = ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  2 2 2, Geo.newVertex 3 3 3]):: Either Hex.HasMeshException L.VertexSafe3List)
      
      assertEqual "create points from 4 vectors safelist fails for non unique vertex"
       (Left(Hex.NonUniqueVertex "non unique safe [Vertex]"))
       (case safeVertexs of
          Right vertexs -> Right $ L.evalSafeList3 vertexs
          Left (Hex.NonUniqueVertex msg) -> Left (Hex.NonUniqueVertex msg)
       )
   )
 runTestTT nonUnique4VertexToPointsFails

 let
  nonUnique5VertexToPointsFails = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let 
        safeVertexs = ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  2 2 2, Geo.newVertex 3 3 3, Geo.newVertex 4 4 4]):: Either Hex.HasMeshException L.VertexSafe3List)
      assertEqual "create points from 5 vectors safelist fails for non unique vertex"
       (Left(Hex.NonUniqueVertex "non unique safe [Vertex]"))
       (case safeVertexs of
          Right vertexs -> Right $ L.evalSafeList3 vertexs
          Left (Hex.NonUniqueVertex msg) -> Left (Hex.NonUniqueVertex msg)
       )
   )
 runTestTT nonUnique5VertexToPointsFails

 let
  nonUnique6VertexToPointsFails = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let 
        safeVertexs = ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  2 2 2,
                                       Geo.newVertex 3 3 3, Geo.newVertex 4 4 4, Geo.newVertex 5 5 5]):: Either Hex.HasMeshException L.VertexSafe3List)
      assertEqual "create points from 6 vectors safelist fails for non unique vertex"
       (Left(Hex.NonUniqueVertex "non unique safe [Vertex]"))
       (case safeVertexs of
          Right vertexs -> Right $ L.evalSafeList3 vertexs
          Left (Hex.NonUniqueVertex msg) -> Left (Hex.NonUniqueVertex msg)
       )
   )
 runTestTT nonUnique6VertexToPointsFails

 let
  nonUnique10VertexToPointsFails = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      let 
        safeVertexs = ((L.toSafeList3 [Geo.newVertex  1 1 1, Geo.newVertex  2 2 2, Geo.newVertex  2 2 2,
                                       Geo.newVertex 3 3 3, Geo.newVertex 4 4 4, Geo.newVertex 5 5 5,
                                       Geo.newVertex 13 13 13, Geo.newVertex 14 14 14, Geo.newVertex 15 15 15, Geo.newVertex 16 16 16]):: Either Hex.HasMeshException L.VertexSafe3List)
      assertEqual "create points from 10 vectors safelist fails for non unique vertex"
       (Left(Hex.NonUniqueVertex "non unique safe [Vertex]"))
       (case safeVertexs of
          Right vertexs -> Right $ L.evalSafeList3 vertexs
          Left (Hex.NonUniqueVertex msg) -> Left (Hex.NonUniqueVertex msg)
       )
   )
 runTestTT nonUnique10VertexToPointsFails


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

