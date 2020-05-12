{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs #-}
module List1Test(runTests) where

import RIO
import Test.HUnit
import qualified Prelude as P
import qualified Utils.Environment as Env
import qualified List.Safe1 as L1
import qualified List.Safe3 as L3
import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR
import qualified Geometry.Vertex as V
import Utils.Add

runTests = do
 P.putStrLn $ "=============== List1 Test ====================="  


 


 let
   safeLast1From10Int = TestCase $ assertEqual
     "last SafeList1 length == 10"
     (Right [1,2,3,4,5,6,7,8,9,10])
     (let
        eitherSafeList = L1.toSafeList1
          [1,2,3,4,5,6,7,8,9,10] :: Either  Hex.HasMeshException  L1.IntSafe1List
      in
        case eitherSafeList of
          Right safeList -> Right $ L1.evalSafeList1 safeList
          Left err -> Left err
     )
 runTestTT safeLast1From10Int


 
 

 -------------------------------------------------- instance of Add ----------------------------------------------------------------

 let
   safeLastFromInt = TestCase $ assertEqual
     "show that can build a safeList1 of int"
     (Right $ [1,2,3])
     (let
        eitherSafeList = L1.toSafeList1 [1,2,3] :: Either  Hex.HasMeshException  L1.IntSafe1List
      in
        case eitherSafeList of
          Right safeList -> Right $ L1.evalSafeList1 safeList
          Left err -> Left err
     )
 runTestTT safeLastFromInt

 let
   addSafeListOf_3_3 = TestCase $ assertEqual
     "add 2 safeList1 lists, lenght == 3 and 3"
     (Right $ [1,2,3,4,5,6])
     (let
        eitherSafeList1 = L1.toSafeList1 [1,2,3] :: Either  Hex.HasMeshException  L1.IntSafe1List
        eitherSafeList2 = L1.toSafeList1 [4,5,6] :: Either  Hex.HasMeshException  L1.IntSafe1List
      in
        case eitherSafeList1 of
          Right safeList1 ->
            case eitherSafeList2 of
              Right safeList2 ->
                Right $ L1.evalSafeList1 $  safeList1 +++ safeList2
              Left err -> Left err
          Left err -> Left err
     )
 runTestTT addSafeListOf_3_3

 let
   addSafeListOf_4_4 = TestCase $ assertEqual
     "add 2 safeList1 lists, lenght == 4 and 4"
     (Right $ [1,2,3,4,5,6,7,8])
     (let
        eitherSafeList1 = L1.toSafeList1 [1,2,3,4] :: Either  Hex.HasMeshException  L1.IntSafe1List
        eitherSafeList2 = L1.toSafeList1 [5,6,7,8] :: Either  Hex.HasMeshException  L1.IntSafe1List
      in
        case eitherSafeList1 of
          Right safeList1 ->
            case eitherSafeList2 of
              Right safeList2 ->
                Right $ L1.evalSafeList1 $  safeList1 +++ safeList2
              Left err -> Left err
          Left err -> Left err
     )
 runTestTT addSafeListOf_4_4


 

 

