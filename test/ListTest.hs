{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs #-}
module ListTest(runTests) where

import RIO
import Test.HUnit
import qualified Prelude as P
import qualified Gmsh.ID as ID
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex

import qualified Utils.RunExceptions as HexR
import qualified Geometry.Vertex as V

runTests = do
 P.putStrLn $ "=============== List Test ====================="  

 -- ================================================== toSafeList3 =================================================
      
 let
  pointId1 = ID.initialId
  pointId2 = ID.incr pointId1
  buildPointIdSafe3ListFrom2PointIdFails = TestCase $ assertEqual
   "build a PointIdSafe3List from < 3 ID.PointId ID.PointInt throws and exception"
   (Left (Hex.SafeList3MinError "length == 2"))
   (L.toSafeList3 [pointId1, pointId2]::Either Hex.HasMeshException L.PointIdSafe3List)
 _ <- runTestTT buildPointIdSafe3ListFrom2PointIdFails
 
 let
  pointId11 = ID.initialId
  pointId12 = ID.incr pointId1
  pointId13 = ID.incr pointId2
  buildSafe3ListFrom3Points = TestCase $ assertEqual
   "build a PointIdSafe3List from 3 ID.PointId ID.PointInt"
   (Right (L.Cons pointId11 pointId12 pointId13 [] L.Nil::L.PointIdSafe3List))
   (L.toSafeList3 [pointId11, pointId12, pointId13])
 _ <- runTestTT buildSafe3ListFrom3Points
 
 -- ================================================== eval =================================================

 
 let
  evalSafe3List = TestCase $ assertEqual
   "evalSafeList3 . toSafeList == manual L.Cons"
   (Right [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3])
   (case L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3] of
      Right vertexsSafeList' -> Right $ L.evalSafeList3 vertexsSafeList'
      Left (Hex.SafeList3MinError msg) -> Left $ show (Hex.SafeList3MinError msg)
      Left msg -> Left $ show  msg
   )
 _ <- runTestTT evalSafe3List
 
 
 -- ================================================== reverse =================================================
 --due to long pattern matching, need to test beyond 6 vertex:vs

 let
   reverseASafeListOf3Items = TestCase $ assertEqual
    "reverse a SafeList3 of length 3"
    (L.toSafeList3 [V.newVertex 3 3 3, V.newVertex 2 2 2, V.newVertex 1 1 1] ::Either Hex.HasMeshException L.VertexSafe3List)
    (
       let eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3 ]
       in
       case eitherSafeList of
         Right rightSafeList ->  Right $ L.reverseSafeList3 rightSafeList
         Left (Hex.SafeList3MinError msg) -> Left $  Hex.SafeList3MinError msg
         Left _ -> Left $  Hex.SafeList3MinError "cover all pattern matches"
    )
 _ <- runTestTT reverseASafeListOf3Items
 
 let
   reverseASafeListOf4Items = TestCase $ assertEqual
    "reverse a SafeList3 of length 3"
    (L.toSafeList3 [V.newVertex 4 4 4, V.newVertex 3 3 3, V.newVertex 2 2 2, V.newVertex 1 1 1] ::Either Hex.HasMeshException L.VertexSafe3List)
    (
       let eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4]
       in
       case eitherSafeList of
         Right rightSafeList ->  Right $ L.reverseSafeList3 rightSafeList
         Left (Hex.SafeList3MinError msg) -> Left $  Hex.SafeList3MinError msg
         Left _ -> Left $  Hex.SafeList3MinError "cover all pattern matches"
    )
 _ <- runTestTT reverseASafeListOf4Items

 let
   reverseASafeListOf5Items = TestCase $ assertEqual
    "reverse a SafeList3 of length 3"
    (L.toSafeList3 [V.newVertex 5 5 5, V.newVertex 4 4 4, V.newVertex 3 3 3, V.newVertex 2 2 2, V.newVertex 1 1 1] ::Either Hex.HasMeshException L.VertexSafe3List)
    (
       let eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4, V.newVertex 5 5 5]
       in
       case eitherSafeList of
         Right rightSafeList ->  Right $ L.reverseSafeList3 rightSafeList
         Left (Hex.SafeList3MinError msg) -> Left $  Hex.SafeList3MinError msg
         Left _ -> Left $  Hex.SafeList3MinError "cover all pattern matches"
    )
 _ <- runTestTT reverseASafeListOf5Items

 let
   reverseASafeListOf6Items = TestCase $ assertEqual
    "reverse a SafeList3 of length 3"
    (L.toSafeList3 [V.newVertex 6 6 6, V.newVertex 5 5 5, V.newVertex 4 4 4, V.newVertex 3 3 3, V.newVertex 2 2 2, V.newVertex 1 1 1] ::Either Hex.HasMeshException L.VertexSafe3List)
    (
       let eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4, V.newVertex 5 5 5, V.newVertex 6 6 6]
       in
       case eitherSafeList of
         Right rightSafeList ->  Right $ L.reverseSafeList3 rightSafeList
         Left (Hex.SafeList3MinError msg) -> Left $  Hex.SafeList3MinError msg
         Left _ -> Left $  Hex.SafeList3MinError "cover all pattern matches"
    )
 _ <- runTestTT reverseASafeListOf6Items

 let
   reverseASafeListOf7Items = TestCase $ assertEqual
    "reverse a SafeList3 of length 3"
    (L.toSafeList3 [V.newVertex 7 7 7, V.newVertex 6 6 6, V.newVertex 5 5 5, V.newVertex 4 4 4, V.newVertex 3 3 3, V.newVertex 2 2 2, V.newVertex 1 1 1] ::Either Hex.HasMeshException L.VertexSafe3List)
    (
       let eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4, V.newVertex 5 5 5, V.newVertex 6 6 6, V.newVertex 7 7 7]
       in
       case eitherSafeList of
         Right rightSafeList ->  Right $ L.reverseSafeList3 rightSafeList
         Left (Hex.SafeList3MinError msg) -> Left $  Hex.SafeList3MinError msg
         Left _ -> Left $  Hex.SafeList3MinError "cover all pattern matches"
    )
 _ <- runTestTT reverseASafeListOf7Items

 let
   reverseASafeListOf8Items = TestCase $ assertEqual
    "reverse a SafeList3 of length 3"
    (L.toSafeList3 [V.newVertex 8 8 8, V.newVertex 7 7 7, V.newVertex 6 6 6, V.newVertex 5 5 5, V.newVertex 4 4 4, V.newVertex 3 3 3, V.newVertex 2 2 2, V.newVertex 1 1 1] ::Either Hex.HasMeshException L.VertexSafe3List)
    (
       let eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4, V.newVertex 5 5 5, V.newVertex 6 6 6, V.newVertex 7 7 7, V.newVertex 8 8 8]
       in
       case eitherSafeList of
         Right rightSafeList ->  Right $ L.reverseSafeList3 rightSafeList
         Left (Hex.SafeList3MinError msg) -> Left $  Hex.SafeList3MinError msg
         Left _ -> Left $  Hex.SafeList3MinError "cover all pattern matches"
    )
 _ <- runTestTT reverseASafeListOf8Items


  -- ================================================== append =================================================
 let
   appendToASafeListOf3Items = TestCase $ assertEqual
    "append onto a SafeList3 of length 3"
    (L.toSafeList3 [V.newVertex 0 0 0, V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3 ] ::Either Hex.HasMeshException L.VertexSafe3List)
    (
       let eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3 ]
       in
       case eitherSafeList of
         Right rightSafeList ->  Right $ L.appendSafeList3 (V.newVertex 0 0 0) rightSafeList
         Left (Hex.SafeList3MinError msg) -> Left $  Hex.SafeList3MinError msg
         Left _ -> Left $  Hex.SafeList3MinError "cover all pattern matches"
    )
 _ <- runTestTT appendToASafeListOf3Items
 
-------------------------------- safehead --------------------------------
 let
   getSafeHead = TestCase $ assertEqual
     "head of SafeList3 length == 3"
     (Right $ V.newVertex 1 1 1)
     (let
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $  L.safeHead3 safeList
          Left err -> Left err
     )
 _ <- runTestTT getSafeHead
 
---------------------------------safeLast ---------------------------------
 let
   safeLastFrom3Vertex = TestCase $ assertEqual
     "last SafeList3 length == 3"
     (Right $ V.newVertex 3 3 3)
     (let
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1,V.newVertex 2 2 2, V.newVertex 3 3 3] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.safeLast3 safeList
          Left err -> Left err
     )
 _ <- runTestTT safeLastFrom3Vertex

 let
   safeLastFrom4Vertex = TestCase $ assertEqual
     "last SafeList3 length == 4"
     (Right $ V.newVertex 4 4 4)
     (let
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1,V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.safeLast3 safeList
          Left err -> Left err
     )
 _ <- runTestTT safeLastFrom4Vertex

 let
   safeLastFrom5Vertex = TestCase $ assertEqual
     "last SafeList3 length == 5"
     (Right $ V.newVertex 5 5 5)
     (let
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1,V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4, V.newVertex 5 5 5] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.safeLast3 safeList
          Left err -> Left err
     )
 runTestTT safeLastFrom5Vertex
 

 

 

