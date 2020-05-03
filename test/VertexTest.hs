{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs #-}
module VertexTest(runTests) where
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO

import qualified Geometry.Vertex as V
import qualified Geometry.Polar as Polar
import qualified Geometry.Axis as Axis
import qualified Utils.Environment as Env
import qualified Utils.EnvironmentLoader as EnvLdr 
import qualified Geometry.Geometry as Geo
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR

runTests = do
 P.putStrLn $ "=============== VertexTest ====================="  
-- ============================= Eq ==========================================
 let
  testEq1 = TestCase $ assertEqual
   "Vectors are equal"
   True
   (V.newVertex 1 2 3 == V.newVertex 1 2 3)
 _ <- runTestTT testEq1
 
 let
   testEq2 = TestCase $ assertEqual
     "Vectors are not equal"
     False
     (V.newVertex 1 2 3 == V.newVertex 11 2 3)
 _ <- runTestTT testEq2
 

 let
   testEq3 = TestCase $ assertEqual
     "Vectors are not equal"
     False
     (V.newVertex 1 2 3 == V.newVertex 1 22 3)
 _ <- runTestTT testEq3
 
 let
   testEq4 = TestCase $ assertEqual
     "Vectors are not equal"
      False
      (V.newVertex 1 2 3 == V.newVertex 1 2 33)
 _ <- runTestTT testEq4
 
 let
   testEq5 = TestCase $ assertEqual
     "Vectors are equal within 1/100th"
      True
      (V.newVertex 1.111 2 3 == V.newVertex 1.110 2 3)
 _ <- runTestTT testEq5
 

 let
   testEq6 = TestCase $ assertEqual
     "Vectors are not equal within 1/100th"
      False
      (V.newVertex 1.1 2 3 == V.newVertex 1.11 2 3)
 _ <- runTestTT testEq6
 

 let
   testEq7 = TestCase $ assertEqual
     "Vectors are equal at zero"
      True
      (V.newVertex 0 0 0 == V.newVertex 0 0 0)
 _ <- runTestTT testEq7
 
 let
   testEq8 = TestCase $ assertEqual
     "Vectors are not equal at if not wihtin 1/100th"
      False
      (V.newVertex 0 0 0 == V.newVertex 0 0 0.01)
 _ <- runTestTT testEq8
 
-- ===================================== hashable ================================
 let
   testHashing1 = TestCase $ assertEqual
     "Vector 0 0 0 is hashed"
      (-3771506651183149892)
      (H.hash (V.newVertex 0 0 0) )
 _ <- runTestTT testHashing1
 
 let
   testHashing2 = TestCase $ assertEqual
     "Vector of neg numbers is hashed"
      (-6664453513253834666)
      (H.hash (V.newVertex (-1) (-2) (-3)) )
 _ <- runTestTT testHashing2

 let
   testHashing3 = TestCase $ assertEqual
     "Vector hashes are not equal at if not wihtin 1/100th"
      False
      (H.hash (V.newVertex 0 0 0) == H.hash (V.newVertex 0 0 0.01))
 _ <- runTestTT testHashing3

 let
   testHashing4 = TestCase $ assertEqual
     "Vector hashes are equal within 1/100th"
      True
      (H.hash (V.newVertex 1.111 2 3) == H.hash (V.newVertex 1.110 2 3))
 _ <- runTestTT testHashing4

 -- =========================================== mapable ===============================================
 let
   createVertexMap = TestCase $ assertEqual
     "Stick a Vector into a map, using its hash value as key"
      (Map.fromList [(H.hash $ V.newVertex 1 2 3 , Env.initialId)]::Map Int (Env.Id Env.PointInt))
      (let
          newMap = Map.empty
          vertex = V.newVertex 1 2 3
          hashed = H.hash vertex
       in
         Map.insert hashed Env.initialId newMap
      )
 _ <- runTestTT createVertexMap
 
                

 -- ============================== use a map inside an IORef ==============================
 let
  --emptyMap = Map.empty
  insertNewPointIntoIORefPointMap = TestCase 
   (do
      newIOMap <- newIORef (Map.insert  (H.hash $ V.newVertex 1 1 1) Env.initialId Map.empty::(Map Int (Env.Id Env.PointInt)))
      newMap <- readIORef newIOMap 
      writeIORef newIOMap (Map.insert (H.hash $ V.newVertex 2 2 2) (Env.incr Env.initialId) newMap)
      mapWith2ndVertexAdded <- readIORef newIOMap
      assertEqual "insert a new vertex and PointId into an IORef map" (Map.fromList [(H.hash $ V.newVertex 1 1 1 , Env.initialId), (H.hash $ V.newVertex 2 2 2  , Env.incr Env.initialId)]) mapWith2ndVertexAdded
      
   )
 _ <- runTestTT insertNewPointIntoIORefPointMap
 
-- ============================================ cx isOpen ================================================
-- isOpen should be replace by having no duplicates, isUnique.
 let
   isOpenSafeList3Test = TestCase $ assertEqual
     "isOpen SafeList3 length == 3"
     (Right True)
     (let
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isOpen safeList
          Left err -> Left err
     )
 _ <- runTestTT isOpenSafeList3Test

{- fails because of new isUnique. Keep here till isUnique system is done.
 let
   isNotOpenSafeList3Test = TestCase $ assertEqual
     "not isOpen SafeList3 length == 3"
     (Right False)
     (let
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 1 1 1] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isOpen safeList
          Left err -> Left err
     )
 _ <- runTestTT isNotOpenSafeList3Test
-}


 let
   isOpenSafeList4Test = TestCase $ assertEqual
     "isOpen SafeList3 length == 4"
     (Right True)
     (let                                                   
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 4 4 4] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isOpen safeList
          Left err -> Left err
     )
 _ <- runTestTT isOpenSafeList4Test
{- fails because of new isUnique. Keep here till isUnique system is done.
 let
   isNotOpenSafeList4Test = TestCase $ assertEqual
     "not isOpen SafeList3 length == 4"
     (Right False)
     (let                               
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 1 1 1] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isOpen safeList
          Left err -> Left err
     )
 runTestTT isNotOpenSafeList4Test
-}

-- ============================================ cx isUnique ================================================
-- The toSafeList will construct a unique list of vertex, or return an exception.


--------------- first create a unique fx that will be used by VertexSafe3List constructor--------------
 let
   isUniqueSafeList = TestCase $ assertEqual
     "isUnique SafeList3 length == 3"
     (Right True)
     (let                               
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isUnique safeList
          Left err -> Left err
     )
 runTestTT isUniqueSafeList

{- fails because of new isUnique. Keep here till isUnique system is done.
 let
   isNotUniqueSafeList = TestCase $ assertEqual
     "not isUnique SafeList3 length == 3"
     (Right False)
     (let                               
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 1 1 1] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isUnique safeList
          Left err -> Left err
     )
 runTestTT isNotUniqueSafeList
-}
 let
   isNotUniqueSafeList4 = TestCase $ assertEqual
     "not isUnique SafeList3 length == 4"
     (Left (Hex.NonUniqueVertex "non unique safe [Vertex]"))
     (let                               
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 1 1 1, V.newVertex 4 4 4] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isUnique safeList
          Left (Hex.SafeList3MinError msg) -> Left $ Hex.SafeList3MinError msg
          Left (Hex.NonUniqueVertex msg) -> Left $ Hex.NonUniqueVertex msg
          Left err -> Left err
     )
 runTestTT isNotUniqueSafeList4


--------------- check that VertexSafe3List constructor creates a unique list--------------
 let
   nonUniqueSafeListOf3ReturnsEx = TestCase $ assertEqual
     "not isUnique SafeList3 returns an exception"
     (Left (Hex.NonUniqueVertex "non unique safe [Vertex]"))
     (let                               
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 2 2 2] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isUnique safeList
          Left (Hex.SafeList3MinError msg) -> Left $ Hex.SafeList3MinError msg
          Left (Hex.NonUniqueVertex msg) -> Left $ Hex.NonUniqueVertex msg
          Left err -> Left err
     )
 runTestTT nonUniqueSafeListOf3ReturnsEx

 let
   nonUniqueSafeListOf4ReturnsEx = TestCase $ assertEqual
     "not isUnique SafeList3 returns an exception"
     (Left (Hex.NonUniqueVertex "non unique safe [Vertex]"))
     (let                               
        eitherSafeList = L.toSafeList3 [V.newVertex 1 1 1, V.newVertex 2 2 2, V.newVertex 3 3 3, V.newVertex 2 2 2] :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isUnique safeList
          Left (Hex.SafeList3MinError msg) -> Left $ Hex.SafeList3MinError msg
          Left (Hex.NonUniqueVertex msg) -> Left $ Hex.NonUniqueVertex msg
          Left err -> Left err
     )
 runTestTT nonUniqueSafeListOf4ReturnsEx


