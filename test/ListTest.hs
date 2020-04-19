{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ListTest(runTests) where

import RIO
import Test.HUnit
import qualified Prelude as P
import qualified Gmsh.ID as ID
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex
import qualified Geometry.Vertex as V

runTests = do
 P.putStrLn $ "=============== List Test ====================="  

 -- ================================================== toSafeList3 =================================================
 let
  pointIds = [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3)
                  ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --Build a L.PointIdSafe3List from a [ID.PointId $ ID.PointInt ]
  manuallyBuildPointIdSafe3List = TestCase $ assertEqual
   "toSafeList == manual L.Cons"
   (Right $ manuallyBuildSafe3List pointIds )
   (--If it was not being used to compare to `manuallyBuildSafe3List` then it will not compile due to ambigous type.
    --However, I have to let gch infer it, as I can't get the right type to compile.
    L.toSafeList3 pointIds
   )
      
 runTestTT manuallyBuildPointIdSafe3List

 let
  vertex1 = V.newVertex 1 1 1
  vertex2 = V.newVertex 2 2 2
  vertex3 = V.newVertex 3 3 3
  vertexs = [vertex1,vertex2,vertex3]
  vertexsSafeList = L.toSafeList3 vertexs
  --Build a L.PointIdSafe3List from a [ID.PointId $ ID.PointInt ]
  vertexToSafe3List = TestCase $ assertEqual
   "evalSafeList3 . toSafeList == manual L.Cons"
   (Right vertexs)
   (case vertexsSafeList of
      Right vertexsSafeList' -> Right $ L.evalSafeList3 vertexsSafeList'
      Left msg -> Left "error"
   )
 runTestTT vertexToSafe3List


 -- ================================================== reverse =================================================
 let
  reversedPointIds = [(ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 1)
                  ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --Build a L.PointIdSafe3List from a [ID.PointId $ ID.PointInt ]
  reverseASafeListOf3Items = TestCase $ assertEqual
   "reverse a SafeList3 of length 3"
   (manuallyBuildSafe3List reversedPointIds )
   (let
       pointIds = [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3)
                  ]
                  
       fromEither :: Either Hex.HasMeshException b -> b
       fromEither (Right val) = val
    in
    L.reverseSafeList3 $ fromEither $ L.toSafeList3 pointIds
   )
      
 runTestTT reverseASafeListOf3Items

 let
  reversedPointIds = [
                      (ID.PointId $ ID.PointInt 4),
                      (ID.PointId $ ID.PointInt 3),
                      (ID.PointId $ ID.PointInt 2),
                      (ID.PointId $ ID.PointInt 1)
                     ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --Build a L.PointIdSafe3List from a [ID.PointId $ ID.PointInt ]
  reverseASafeListOf4Items = TestCase $ assertEqual
   "reverse a SafeList3 of length 4"
   (manuallyBuildSafe3List reversedPointIds )
   (let
       pointIds = [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4)
                  ]
                  
       fromEither :: Either Hex.HasMeshException b -> b
       fromEither (Right val) = val
    in
    L.reverseSafeList3 $ fromEither $ L.toSafeList3 pointIds
   )
      
 runTestTT reverseASafeListOf4Items

 let
  reversedPointIds = [
                      
                      (ID.PointId $ ID.PointInt 5),
                      (ID.PointId $ ID.PointInt 4),
                      (ID.PointId $ ID.PointInt 3),
                      (ID.PointId $ ID.PointInt 2),
                      (ID.PointId $ ID.PointInt 1)
                     ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --Build a L.PointIdSafe3List from a [ID.PointId $ ID.PointInt ]
  reverseASafeListOf5Items = TestCase $ assertEqual
   "reverse a SafeList3 of length 5"
   (manuallyBuildSafe3List reversedPointIds )
   (let
       pointIds = [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4),
                   (ID.PointId $ ID.PointInt 5)
                   
                  ]
                  
       fromEither :: Either Hex.HasMeshException b -> b
       fromEither (Right val) = val
    in
    L.reverseSafeList3 $ fromEither $ L.toSafeList3 pointIds
   )
      
 runTestTT reverseASafeListOf5Items
 

 let
  reversedPointIds = [
                      (ID.PointId $ ID.PointInt 6),
                      (ID.PointId $ ID.PointInt 5),
                      (ID.PointId $ ID.PointInt 4),
                      (ID.PointId $ ID.PointInt 3),
                      (ID.PointId $ ID.PointInt 2),
                      (ID.PointId $ ID.PointInt 1)
                     ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --Build a L.PointIdSafe3List from a [ID.PointId $ ID.PointInt ]
  reverseASafeListOf6Items = TestCase $ assertEqual
   "reverse a SafeList3 of length 6"
   (manuallyBuildSafe3List reversedPointIds )
   (let
       pointIds = [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4),
                   (ID.PointId $ ID.PointInt 5),
                   (ID.PointId $ ID.PointInt 6)
                  ]
                  
       fromEither :: Either Hex.HasMeshException b -> b
       fromEither (Right val) = val
    in
    L.reverseSafeList3 $ fromEither $ L.toSafeList3 pointIds
   )
      
 runTestTT reverseASafeListOf6Items

 let
  reversedPointIds = [(ID.PointId $ ID.PointInt 7),
                      (ID.PointId $ ID.PointInt 6),
                      (ID.PointId $ ID.PointInt 5),
                      (ID.PointId $ ID.PointInt 4),
                      (ID.PointId $ ID.PointInt 3),
                      (ID.PointId $ ID.PointInt 2),
                      (ID.PointId $ ID.PointInt 1)
                     ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --Build a L.PointIdSafe3List from a [ID.PointId $ ID.PointInt ]
  reverseASafeListOf7Items = TestCase $ assertEqual
   "reverse a SafeList3 of length 7"
   (manuallyBuildSafe3List reversedPointIds )
   (let
       pointIds = [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4),
                   (ID.PointId $ ID.PointInt 5),
                   (ID.PointId $ ID.PointInt 6),
                   (ID.PointId $ ID.PointInt 7)
                  ]
                  
       fromEither :: Either Hex.HasMeshException b -> b
       fromEither (Right val) = val
    in
    L.reverseSafeList3 $ fromEither $ L.toSafeList3 pointIds
   )
      
 runTestTT reverseASafeListOf7Items

 runTestTT reverseASafeListOf6Items

 let
  reversedPointIds = [(ID.PointId $ ID.PointInt 17),
                      (ID.PointId $ ID.PointInt 16),
                      (ID.PointId $ ID.PointInt 15),
                      (ID.PointId $ ID.PointInt 14),
                      (ID.PointId $ ID.PointInt 13),
                      (ID.PointId $ ID.PointInt 12),
                      (ID.PointId $ ID.PointInt 11),
                      (ID.PointId $ ID.PointInt 7),
                      (ID.PointId $ ID.PointInt 6),
                      (ID.PointId $ ID.PointInt 5),
                      (ID.PointId $ ID.PointInt 4),
                      (ID.PointId $ ID.PointInt 3),
                      (ID.PointId $ ID.PointInt 2),
                      (ID.PointId $ ID.PointInt 1)
                     ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --Build a L.PointIdSafe3List from a [ID.PointId $ ID.PointInt ]
  reverseASafeListOf14Items = TestCase $ assertEqual
   "reverse a SafeList3 of length 14"
   (manuallyBuildSafe3List reversedPointIds )
   (let
       pointIds = [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4),
                   (ID.PointId $ ID.PointInt 5),
                   (ID.PointId $ ID.PointInt 6),
                   (ID.PointId $ ID.PointInt 7),
                   (ID.PointId $ ID.PointInt 11),
                   (ID.PointId $ ID.PointInt 12),
                   (ID.PointId $ ID.PointInt 13),
                   (ID.PointId $ ID.PointInt 14),
                   (ID.PointId $ ID.PointInt 15),
                   (ID.PointId $ ID.PointInt 16),
                   (ID.PointId $ ID.PointInt 17)
                  ]
                  
       fromEither :: Either Hex.HasMeshException b -> b
       fromEither (Right val) = val
    in
    L.reverseSafeList3 $ fromEither $ L.toSafeList3 pointIds
   )
      
 runTestTT reverseASafeListOf14Items

  -- ================================================== append =================================================
 let
  pointIds = [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4)
                  ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --append a pointId to a PointIdSafe3List
  appendPointIdToAPointIdSafe3List = TestCase $ assertEqual
   "appendSafeList3"
   (manuallyBuildSafe3List pointIds )
   (let
       pointIds = [
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4)
                  ]
                  
       fromEither :: Either Hex.HasMeshException b -> b
       fromEither (Right val) = val
    in
      L.appendSafeList3 (ID.PointId $ ID.PointInt 1) $ fromEither $ L.toSafeList3 pointIds
   )
      
 runTestTT appendPointIdToAPointIdSafe3List

 let
  pointIds =      [(ID.PointId $ ID.PointInt 1),
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4),
                   (ID.PointId $ ID.PointInt 5),
                   (ID.PointId $ ID.PointInt 6),
                   (ID.PointId $ ID.PointInt 7),
                   (ID.PointId $ ID.PointInt 8),
                   (ID.PointId $ ID.PointInt 9),
                   (ID.PointId $ ID.PointInt 10)
                  ]
  manuallyBuildSafe3List :: [ID.Id ID.PointInt] -> L.PointIdSafe3List 
  manuallyBuildSafe3List (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
       
  --append a pointId to a PointIdSafe3List
  appendPointIdToAPointIdSafe3ListOf10 = TestCase $ assertEqual
   "appendSafeList3"
   (manuallyBuildSafe3List pointIds )
   (let
       pointIds = [
                   (ID.PointId $ ID.PointInt 2),
                   (ID.PointId $ ID.PointInt 3),
                   (ID.PointId $ ID.PointInt 4),
                   (ID.PointId $ ID.PointInt 5),
                   (ID.PointId $ ID.PointInt 6),
                   (ID.PointId $ ID.PointInt 7),
                   (ID.PointId $ ID.PointInt 8),
                   (ID.PointId $ ID.PointInt 9),
                   (ID.PointId $ ID.PointInt 10)
                  ]
                  
       fromEither :: Either Hex.HasMeshException b -> b
       fromEither (Right val) = val
    in
      L.appendSafeList3 (ID.PointId $ ID.PointInt 1) $ fromEither $ L.toSafeList3 pointIds
   )
      
 runTestTT appendPointIdToAPointIdSafe3ListOf10
{-

-}
