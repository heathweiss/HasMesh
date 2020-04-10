{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module VertexTest(runTests) where
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO

import qualified Geometry.Vertex as V
import qualified Gmsh.Gmsh as Gmsh
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Point as Pts  
import qualified Utils.EnvironmentLoader as EnvLdr
import qualified Utils.Environment as Enviro
import qualified Utils.List as L


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
        workInRIO :: (Enviro.HasPointIdSupply env) => RIO env (Gmsh.Id (Gmsh.PointInt))
        workInRIO = do
          pointIdRef <- view Enviro.pointIdSupplyL
          currId <- readIORef pointIdRef
          writeIORef pointIdRef (Gmsh.incr currId )
          finalId <- readIORef pointIdRef
          return (finalId)
      
      env <- EnvLdr.loadEnvironment
      result <- runRIO env workInRIO 
      assertEqual "get the vector id from an ioref" (Gmsh.PointId $ Gmsh.PointInt 2) result 
   )
 runTestTT testGetVertexId2


{- 
--Load an environment in IO, then insert 2 Vertexs that are in an array
 let
  testGetVertexIdsUsingRIO3 = TestCase
   (do
      
      
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6]
      points <- runRIO env $ Pts.toPoints vertexs
      assertEqual "get the vector id from an ioref" [Gmsh.PointId $ Gmsh.PointInt 1, Gmsh.PointId $ Gmsh.PointInt 2] points -- result 
   )
 runTestTT testGetVertexIdsUsingRIO3
-}


--Load an environment in IO, then create 3 lines from 3 Vertexs that are in an array, using Pts.toPointsT
 let
  testGetVertexIdsUsingRIO3T = TestCase
   (do
      
      
      env <- EnvLdr.loadTestEnvironment
      let
        vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6]
      points <- runRIO env $ Pts.toPoints vertexs
      --assertEqual "get the vector id from an ioref" [Gmsh.PointId $ Gmsh.PointInt 1, Gmsh.PointId $ Gmsh.PointInt 2] points -- result
      assertEqual "get the vector id from an ioref" (Right $ L.Cons (Gmsh.PointId $ Gmsh.PointInt 1) (Gmsh.PointId $ Gmsh.PointInt 2) []  L.Nil) points -- result 
   )
 runTestTT testGetVertexIdsUsingRIO3T


-- =========================================== get PointId and write to handle ===========================
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
