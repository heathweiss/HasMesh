{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module VectorTest(runTests) where
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map

import Test.HUnit
import qualified Geometry.Vector as V
import qualified Geometry.ID as ID
import qualified Data.Hashable as H

runTests = do
-- ============================= Eq ==========================================
 let
  testEq1 = TestCase $ assertEqual
   "Vectors are equal"
   (True)
   ((V.newVertex 1 2 3) == (V.newVertex 1 2 3))
 runTestTT testEq1

 let
   testEq2 = TestCase $ assertEqual
     "Vectors are not equal"
     (False)
     ((V.newVertex 1 2 3) == (V.newVertex 11 2 3))
 runTestTT testEq2

 let
   testEq3 = TestCase $ assertEqual
     "Vectors are not equal"
     (False)
     ((V.newVertex 1 2 3) == (V.newVertex 1 22 3))
 runTestTT testEq3
 
 let
   testEq4 = TestCase $ assertEqual
     "Vectors are not equal"
      (False)
      ((V.newVertex 1 2 3) == (V.newVertex 1 2 33))
 runTestTT testEq4

 let
   testEq5 = TestCase $ assertEqual
     "Vectors are equal within 1/100th"
      (True)
      ((V.newVertex 1.111 2 3) == (V.newVertex 1.110 2 3))
 runTestTT testEq5

 let
   testEq6 = TestCase $ assertEqual
     "Vectors are not equal within 1/100th"
      (False)
      ((V.newVertex 1.1 2 3) == (V.newVertex 1.11 2 3))
 runTestTT testEq6

 let
   testEq7 = TestCase $ assertEqual
     "Vectors are equal at zero"
      (True)
      ((V.newVertex 0 0 0) == (V.newVertex 0 0 0))
 runTestTT testEq7

 let
   testEq8 = TestCase $ assertEqual
     "Vectors are not equal at if not wihtin 1/100th"
      (False)
      ((V.newVertex 0 0 0) == (V.newVertex 0 0 0.01))
 runTestTT testEq8

 -- ===================================== hashable ================================
 let
   testHashing1 = TestCase $ assertEqual
     "Vector 0 0 0 is hashed"
      (340311421221253963)
      (H.hash (V.newVertex 0 0 0) )
 runTestTT testHashing1

 let
   testHashing2 = TestCase $ assertEqual
     "Vector of neg numbers is hashed"
      (-7052347367107415221)
      (H.hash (V.newVertex (-1) (-2) (-3)) )
 runTestTT testHashing2

 let
   testHashing3 = TestCase $ assertEqual
     "Vector hashes are not equal at if not wihtin 1/100th"
      (False)
      ((H.hash (V.newVertex 0 0 0)) == (H.hash(V.newVertex 0 0 0.01)))
 runTestTT testHashing3

 let
   testHashing4 = TestCase $ assertEqual
     "Vector hashes are equal within 1/100th"
      (True)
      ((H.hash $ V.newVertex 1.111 2 3) == (H.hash $ V.newVertex 1.110 2 3))
 runTestTT testHashing4


 -- =========================================== mapable ===============================================
 let
   testMapping1 = TestCase $ assertEqual
     "Stick a Vector into a map"
      (Map.fromList [(H.hash $ V.newVertex 1 2 3 , (ID.PointId 1))])
      (let
          map = Map.empty
          vertex = V.newVertex 1 2 3
          hashed = H.hash vertex
       in
         Map.insert hashed (ID.PointId 1) map
         
      )
 runTestTT testMapping1
 

 -- ============================== use a map inside an IORef ==============================
 --Just get a map into an IORef.
 let
  map = Map.empty
  vertex = V.newVertex 1 2 3
  hashed = H.hash vertex
  vertex2 = V.newVertex 1 22 3
  hashed2 = H.hash vertex2
  testMapping2 = TestCase 
   (do
      ioref <- newIORef $ Map.insert hashed (ID.PointId 1) map
      x <- readIORef ioref
      writeIORef ioref (Map.insert hashed2 (ID.PointId 2) x)
      y <- readIORef ioref
      assertEqual "put a map into a IOVar" (Map.fromList [(H.hash $ V.newVertex 1 2 3 , (ID.PointId 1)), (H.hash $ V.newVertex 1 22 3 , (ID.PointId 2))]) y 
   )
 runTestTT testMapping2

  --Put a hashed vertex in a map, which is in an IORef. Then remove it in another fx which the IORef is passed into.
  --See that the changes to IORef persist outside the called fx.
 let
  map = Map.empty
  vertex = V.newVertex 1 2 3
  hashed = H.hash vertex
  vertex2 = V.newVertex 1 22 3
  hashed2 = H.hash vertex2
  --modifier :: IO ()
  modifier ref = do
    let
      vertex = V.newVertex 1 2 33
      hashed = H.hash vertex
    x <- readIORef ref
    writeIORef ref (Map.delete hashed2 x)
    --P.putStrLn "hell"
    return ()
  
  testMapping3 = TestCase
   (do
      ioref <- newIORef $ Map.insert hashed (ID.PointId 1) map
      x <- readIORef ioref
      writeIORef ioref (Map.insert hashed2 (ID.PointId 2) x)
      --removed hashed2 from the map in separate function.
      modifier ioref
      y <- readIORef ioref
      
      --notice that hashed2 is no longer in the map.
      assertEqual "Modify a IOVar from another fx" (Map.fromList [(hashed,(ID.PointId 1))]) y 
   )
 runTestTT testMapping3

 -- =================================== pull the id's from a lazy list ===================
    
 --Modify the IORef PointId supply from another function, and show that the changes are persisted in calling fx. 
 let
  testGetVertexId2 = TestCase
   (do
      let
        getSetVectorId ref = do
         currId <- readIORef ref
         writeIORef ref (ID.incr currId )
         return (currId)
      ioref <- newIORef $ ID.PointId 1
      id1 <- getSetVectorId ioref
      id2 <- getSetVectorId ioref
      
      --notice that the IORef vertex id was changed in getSetVectorId.
      assertEqual "get the vector id from an ioref" (ID.PointId 2) id2 
   )
 runTestTT testGetVertexId2

