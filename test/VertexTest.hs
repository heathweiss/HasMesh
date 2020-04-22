{-# LANGUAGE TemplateHaskell #-}
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
import qualified Gmsh.Gmsh as Gmsh
import qualified Geometry.Geometry as Geo
import qualified Utils.EnvironmentLoader as EnvLdr
import qualified Utils.Environment as Enviro
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex


runTests = do
 P.putStrLn $ "=============== VertexTest ====================="  
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
      (-3771506651183149892)
      (H.hash (V.newVertex 0 0 0) )
 runTestTT testHashing1

 let
   testHashing2 = TestCase $ assertEqual
     "Vector of neg numbers is hashed"
      (-6664453513253834666)
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
      (Map.fromList [(H.hash $ V.newVertex 1 2 3 , (Gmsh.PointId $ Gmsh.PointInt 1))])
      (let
          map = Map.empty
          vertex = V.newVertex 1 2 3
          hashed = H.hash vertex
       in
         Map.insert hashed (Gmsh.PointId $ Gmsh.PointInt 1) map
         
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
      ioref <- newIORef $ Map.insert hashed (Gmsh.PointId $ Gmsh.PointInt 1) map
      x <- readIORef ioref
      writeIORef ioref (Map.insert hashed2 (Gmsh.PointId $ Gmsh.PointInt 2) x)
      y <- readIORef ioref
      assertEqual "put a map into a IOVar" (Map.fromList [(H.hash $ V.newVertex 1 2 3 , (Gmsh.PointId $ Gmsh.PointInt 1)), (H.hash $ V.newVertex 1 22 3 , (Gmsh.PointId $ Gmsh.PointInt 2))]) y 
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
      ioref <- newIORef $ Map.insert hashed (Gmsh.PointId $ Gmsh.PointInt 1) map
      x <- readIORef ioref
      writeIORef ioref (Map.insert hashed2 (Gmsh.PointId $ Gmsh.PointInt 2) x)
      --removed hashed2 from the map in separate function.
      modifier ioref
      y <- readIORef ioref
      
      --notice that hashed2 is no longer in the map.
      assertEqual "Modify a IOVar from another fx" (Map.fromList [(hashed,(Gmsh.PointId $ Gmsh.PointInt 1))]) y 
   )
 runTestTT testMapping3

 -- =================================== pull the id's from an IORef using Gmsh.incr ===================
    
 --Overwrite the IORef PointId supply from another function, and show that the changes are persisted in calling fx.
 --Tried to do this with modifyIORef' but would not compile due to occures cx.
 let
  testGetVertexId1 = TestCase
   (do
      let
        getSetVectorId ref = do
         currId <- readIORef ref
         writeIORef ref (Gmsh.incr currId )
         return (currId)
      ioref <- newIORef $ Gmsh.PointId $ Gmsh.PointInt 1
      id1 <- getSetVectorId ioref
      id2 <- getSetVectorId ioref
      
      --notice that the IORef vertex id was changed in getSetVectorId.
      assertEqual "get the vector id from an ioref" (Gmsh.PointId $ Gmsh.PointInt 2) id2 
   )
 runTestTT testGetVertexId1
 

 --Load an environment in IO, and increment the PointId from within a RIO monad.
 let
  testGetVertexId2 = TestCase
   (do
      let
        workInRIO :: (Enviro.HasIdSupply env) => RIO env (Gmsh.Id (Gmsh.PointInt))
        workInRIO = do
          pointIdRef <- view Enviro.pointIdSupplyL
          currId <- readIORef pointIdRef
          writeIORef pointIdRef (Gmsh.incr currId )
          finalId <- readIORef pointIdRef
          return (finalId)
      
      env <- EnvLdr.loadTestEnvironment
      result <- runRIO env workInRIO 
      assertEqual "get the vector id from an ioref" (Gmsh.PointId $ Gmsh.PointInt 2) result 
   )
 runTestTT testGetVertexId2



-- toDo: figure out where this test should go. It is about handles.
 let
  -- Show that a Handle can be set to stdout, for test purposes, so gmsh script are not written to a file.
  testGetVertexIdsUsingRIOAndHandle = TestCase
   (do
      let
        -- set handle to stdout, which is set to open when a program is run.
        h = SIO.stdout
      isOpen <- SIO.hIsOpen h
      assertEqual "try redirecting a Handle" True (isOpen)-- False
   )
 runTestTT testGetVertexIdsUsingRIOAndHandle


----------------------------------------------- use a vertex as a SafeList3-------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------

---------------------------- to safe list: do 2 - 5--------------------

 let
   toSafeList2test = TestCase $ assertEqual
     "creating SafeList3 from 2 vertex throws an error"
     (Left (Hex.SafeList3MinError "length == 2"))
     (let
        safeList = (L.toSafeList3 [(V.newVertex 1 1 1), (V.newVertex 2 2 2)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case safeList of
          Right (L.Cons x y z zs _) -> Right $ L.evalSafeList3 (L.Cons x y z zs L.Nil)
          Left err -> Left err
     )
 runTestTT toSafeList2test

 let
   toSafeList3test = TestCase $ assertEqual
     "toSafeList3test from 3 vertex"
     --(Right $ L.Cons (V.newVertex 1 1 1) (V.newVertex 2 2 2) (V.newVertex 3 3 3) [] L.Nil)
     (Right [(V.newVertex 1 1 1), (V.newVertex 2 2 2), (V.newVertex 3 3 3)])
     (let
        safeList = (L.toSafeList3 [(V.newVertex 1 1 1), (V.newVertex 2 2 2), (V.newVertex 3 3 3)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case safeList of
          Right (L.Cons x y z zs _) -> Right $ L.evalSafeList3 (L.Cons x y z zs L.Nil)
          Left err -> Left err
     )
 runTestTT toSafeList3test

------------------------- reverse 3 - 6 ---------------------------
 let
   reverseSafeList3Test = TestCase $ assertEqual
     "reverse SafeList3 length == 3"
     --(Right $ L.Cons (V.newVertex 1 1 1) (V.newVertex 2 2 2) (V.newVertex 3 3 3) [] L.Nil)
     (Right [(V.newVertex 3 3 3), (V.newVertex 2 2 2) ,(V.newVertex 1 1 1) ])
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1), (V.newVertex 2 2 2), (V.newVertex 3 3 3)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.evalSafeList3  $ L.reverseSafeList3 safeList
          Left err -> Left err
     )
 runTestTT reverseSafeList3Test

 let
   reverseSafeList4Test = TestCase $ assertEqual
     "reverse SafeList3 length == 4"
     (Right [(V.newVertex 4 4 4), (V.newVertex 3 3 3), (V.newVertex 2 2 2) ,(V.newVertex 1 1 1) ])
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1), (V.newVertex 2 2 2), (V.newVertex 3 3 3), (V.newVertex 4 4 4)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.evalSafeList3  $ L.reverseSafeList3 safeList
          Left err -> Left err
     )
 runTestTT reverseSafeList4Test

 let
   reverseSafeList5Test = TestCase $ assertEqual
     "reverse SafeList3 length == 5"
     (Right [(V.newVertex 5 5 5), (V.newVertex 4 4 4), (V.newVertex 3 3 3), (V.newVertex 2 2 2) ,(V.newVertex 1 1 1) ])
     (let
        eitherSafeList =
          (L.toSafeList3
           [(V.newVertex 1 1 1), (V.newVertex 2 2 2), (V.newVertex 3 3 3), (V.newVertex 4 4 4), (V.newVertex 5 5 5)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.evalSafeList3  $ L.reverseSafeList3 safeList
          Left err -> Left err
     )
 runTestTT reverseSafeList5Test

------------------------------ append 3 - 5 ----------------------------------

 let
   appendSafeList3Test = TestCase $ assertEqual
     "append to SafeList3 length == 3"
     (Right [V.newVertex 1 1 1,(V.newVertex 2 2 2), (V.newVertex 3 3 3), (V.newVertex 4 4 4)])
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 2 2 2), (V.newVertex 3 3 3), (V.newVertex 4 4 4)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.evalSafeList3 $  L.appendSafeList3 (V.newVertex 1 1 1) safeList
          Left err -> Left err
     )
 runTestTT appendSafeList3Test


 let
   appendSafeList4Test = TestCase $ assertEqual
     "append to SafeList3 length == 4"
     (Right [V.newVertex 1 1 1,(V.newVertex 2 2 2), (V.newVertex 3 3 3), (V.newVertex 4 4 4), V.newVertex 5 5 5])
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 2 2 2), (V.newVertex 3 3 3), (V.newVertex 4 4 4),V.newVertex 5 5 5]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.evalSafeList3 $  L.appendSafeList3 (V.newVertex 1 1 1) safeList
          Left err -> Left err
     )
 runTestTT appendSafeList4Test

-------------------------------- safehead 4 --------------------------------
 let
   headSafeList3Test = TestCase $ assertEqual
     "head of SafeList3 length == 3"
     (Right $ V.newVertex 1 1 1)
     (let
        eitherSafeList = (L.toSafeList3 [V.newVertex 1 1 1, (V.newVertex 2 2 2), (V.newVertex 3 3 3), (V.newVertex 4 4 4)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $  L.safeHead3 safeList
          Left err -> Left err
     )
 runTestTT headSafeList3Test

 --------------- last 3 - 6 ---------------------------------
 let
   lastSafeList3Test = TestCase $ assertEqual
     "last SafeList3 length == 3"
     (Right $ V.newVertex 3 3 3)
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1),(V.newVertex 2 2 2), (V.newVertex 3 3 3)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.safeLast3 safeList
          Left err -> Left err
     )
 runTestTT lastSafeList3Test

 let
   lastSafeList4Test = TestCase $ assertEqual
     "last SafeList3 length == 4"
     (Right $ V.newVertex 4 4 4)
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1),(V.newVertex 2 2 2), (V.newVertex 3 3 3), V.newVertex 4 4 4]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.safeLast3 safeList
          Left err -> Left err
     )
 runTestTT lastSafeList4Test


 let
   lastSafeList5Test = TestCase $ assertEqual
     "last SafeList3 length == 5"
     (Right $ V.newVertex 5 5 5)
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1),(V.newVertex 2 2 2), (V.newVertex 3 3 3), V.newVertex 4 4 4, V.newVertex 5 5 5]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.safeLast3 safeList
          Left err -> Left err
     )
 runTestTT lastSafeList5Test

----------------------- is open 3 - 5 ---------------------------
 let
   isOpenSafeList3Test = TestCase $ assertEqual
     "isOpen SafeList3 length == 3"
     (Right True)
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1),(V.newVertex 2 2 2), (V.newVertex 3 3 3)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isOpen safeList
          Left err -> Left err
     )
 runTestTT isOpenSafeList3Test

 let
   isNotOpenSafeList3Test = TestCase $ assertEqual
     "not isOpen SafeList3 length == 3"
     (Right False)
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1),(V.newVertex 2 2 2), (V.newVertex 1 1 1)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isOpen safeList
          Left err -> Left err
     )
 runTestTT isNotOpenSafeList3Test

 let
   isOpenSafeList4Test = TestCase $ assertEqual
     "isOpen SafeList3 length == 4"
     (Right True)
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1),(V.newVertex 2 2 2), (V.newVertex 3 3 3), V.newVertex 4 4 4]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isOpen safeList
          Left err -> Left err
     )
 runTestTT isOpenSafeList4Test

 let
   isNotOpenSafeList4Test = TestCase $ assertEqual
     "not isOpen SafeList3 length == 4"
     (Right False)
     (let
        eitherSafeList = (L.toSafeList3 [(V.newVertex 1 1 1),(V.newVertex 2 2 2), V.newVertex 3 3 3, (V.newVertex 1 1 1)]) :: Either  Hex.HasMeshException  L.VertexSafe3List
      in
        case eitherSafeList of
          Right safeList -> Right $ L.isOpen safeList
          Left err -> Left err
     )
 runTestTT isNotOpenSafeList4Test

 
