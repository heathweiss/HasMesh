{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}



{- | Supply functions to support the  'Gmsh.ID.PointId' ADT and associated classes.

import qualified Gmsh.Point as Pnt  
-}
module Gmsh.Point({-toPoint -} toPoints, PointIdList()) where

import RIO
import qualified Data.Hashable as H
import qualified RIO.Map as Map
import qualified RIO.ByteString as B

import qualified Utils.Environment as Enviro
import qualified Geometry.Geometry as Geo
import qualified Gmsh.ID as ID
--import qualified Scripting.Scripting as Script
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex 

-- Indicates if the returned ID.Id ID.PointInt is a new Id, or it already exsisted.
data PointIdStatus = PointIdAlreadyExisted (ID.Id ID.PointInt) |  PointIdDidNotExist (ID.Id ID.PointInt)

-- Associate a 'Geometry.Vertex.Vertex' with a 'ID.PointId', which is the Id used by Gmsh in scripting.
-- If the vertex already has an Id, just return the Id, otherwise create a new Id, and write it to a .geo file via the env handle.
toPoint :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> RIO env (ID.Id ID.PointInt)
toPoint vertex  = do
  env <- ask
  pointIdStatus <- runRIO env $ getSetVertexId vertex
  case pointIdStatus of
    PointIdAlreadyExisted preExistingPointId -> return preExistingPointId
    PointIdDidNotExist newPointId -> do
      geoFileHandleIORef <- view Enviro.geoFileHandleL
      geoFileHandle <- readIORef geoFileHandleIORef
      B.hPut geoFileHandle $ ScrB.writePoint vertex newPointId
      return newPointId
  where
  --Check to see if the Vertex has already had a PointId assigned to it.
  -- Will know that by seeing if it has been inserted into the point id map: Map Int (ID.Id ID.PointInt)
  -- If not: get a new PointId, and add it to the point id map: Map Int (ID.Id ID.PointInt). Will increment the PointIsSupply as well.
  -- If yes: get/return the existin PointId
  getSetVertexId :: (Enviro.HasPointIdMap env, Enviro.HasIdSupply env) => Geo.Vertex -> RIO env  PointIdStatus
  getSetVertexId doesThisVertexHaveAnId = do
    pointMapIORef <- view Enviro.pointIdMapL
    pointMap <- readIORef pointMapIORef
    case Map.lookup (H.hash doesThisVertexHaveAnId) pointMap of
      Just pointId -> return $ PointIdAlreadyExisted pointId
      Nothing -> do
        env <- ask
        newPointId <- runRIO env getSetPointId 
        runRIO env $ insertNewPointIntoPointMap  doesThisVertexHaveAnId newPointId
        return $ PointIdDidNotExist newPointId
  
  insertNewPointIntoPointMap :: (Enviro.HasPointIdMap env) => Geo.Vertex -> ID.Id ID.PointInt -> RIO env ()
  insertNewPointIntoPointMap vertex_ pointId = do
    pointMapIORef <- view Enviro.pointIdMapL
    pointMap <- readIORef pointMapIORef
    writeIORef pointMapIORef $ Map.insert (H.hash vertex_ ) pointId pointMap
  
  -- Access the IORef (ID.Id ID.PointInt) in the Environment to:
  -- Get the currently available (ID.Id ID.PointInt)
  -- Increment the IORef (ID.Id ID.PointInt)
  getSetPointId :: (Enviro.HasIdSupply env) => RIO env (ID.Id ID.PointInt)
  getSetPointId = do
    poIntIdSupplyioref <- view Enviro.pointIdSupplyL
    currPointId <- readIORef poIntIdSupplyioref
    writeIORef poIntIdSupplyioref (ID.incr currPointId )
    return currPointId

-- | Process a ['Geo.Vertex'] into a 'PointIdList'. Print any new 'ID.PointId' to .geo file.
-- The [PointIdList] will have at least 3 lines to form a closed polygon such as triangle, square, or irregular shape. The 2nd vertex of the last line will be == 1st vertex of 1st line, to form a closed loop.
--
-- Side effects: Makes changes to the PointId supply and 'Geometry.Vertex.Vertex' map IORefs in 'Enviro.Environment'
-- Returns a Left 'Hex.SafeList3MinError' exception is the vertex list length < 3, as a [ID.Id ID.PointInt] must have at least 3 Vertex to form a closed polygon such as triangle, square, or irregular shape.
--
-- toDo: Should the idea of a list of lines being closed be repesented/enforece with a type such as Closed {closedList :: [a]}
-- For now, will simpley return a closed list, without any type or enforcement.
toPoints :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> RIO env (Either Hex.HasMeshException PointIdList)
toPoints [] = return $ Left $ Hex.SafeList3MinError "length == 0"
toPoints [_] = return $ Left $ Hex.SafeList3MinError "length == 1"
toPoints [_,_] = return $ Left $ Hex.SafeList3MinError "length == 2"
toPoints (x:y:ys) = do
  let
    tooIntIdList :: [ID.Id ID.PointInt] -> PointIdList
    tooIntIdList vertex =
      let
        decode :: [ID.Id ID.PointInt] -> PointIdList
        decode (x':y':z':zs') = L.Cons x' y' z' zs'  L.Nil
      in
        decode $ reverse vertex
    toPoints' :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> [Geo.Vertex] -> [ID.Id ID.PointInt] -> RIO env (Either Hex.HasMeshException PointIdList)
    toPoints' _ [] _ = return $ Left $ Hex.SafeList3MinError "length == 0"
    --The last vertex, so get an Id, and create/return the safelist.
    toPoints' initVertex [x'] workingPoints = do
      --runSimpleApp $ logInfo $ displayShow "in toPoints'"
      env <- ask
      xPointId <- runRIO env $ toPoint x'
      if x' == initVertex then
        return $ Right $ tooIntIdList (xPointId:workingPoints)
      else
        do
          initId <- runRIO env $ toPoint initVertex
          return $ Right $ tooIntIdList (initId:xPointId:workingPoints)
    --Not yet the last vertex, so get the Id and continue processing the [vertex]
    toPoints' initVertex (v:vs) workingPoints = do
      env <- ask
      pointId <- runRIO env $ toPoint v
      toPoints' initVertex vs (pointId:workingPoints)
  env <- ask
  runRIO env $ toPoints' x (x:y:ys) []


-- | A 'Utils.List.SafeList3' containing [ID.Id ID.PointInt] for containing a min length of 3 list of Gmsh point Ids.
type PointIdList = L.SafeList3 (ID.Id ID.PointInt) L.NonEmptyID

{-after extration of: reading writing to ioref and handles 
module Gmsh.Point({-toPoint -} toPoints, PointIdList()) where

import RIO
import qualifed Data.Hashable as H
import qualified RIO.Map as Map
import qualified RIO.ByteString as B

import qualified Utils.Environment as Enviro
import qualified Geometry.Geometry as Geo
import qualified Gmsh.ID as ID
--import qualified Scripting.Scripting as Script
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex 

-- Indicates if the returned ID.Id ID.PointInt is a new Id, or it already exsisted.
data PointIdStatus = PointIdAlreadyExisted (ID.Id ID.PointInt) |  PointIdDidNotExist (ID.Id ID.PointInt)

-- Associate a 'Geometry.Vertex.Vertex' with a 'ID.PointId', which is the Id used by Gmsh in scripting.
-- If the vertex already has an Id, just return the Id, otherwise create a new Id, and write it to a .geo file via the env handle.
toPoint :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> RIO env (ID.Id ID.PointInt)
toPoint vertex  = do
  env <- ask
  pointIdStatus <- runRIO env $ getSetVertexId vertex
  case pointIdStatus of
    PointIdAlreadyExisted preExistingPointId -> return preExistingPointId
    PointIdDidNotExist newPointId -> do
      geoFileHandleIORef <- view Enviro.geoFileHandleL
      geoFileHandle <- readIORef geoFileHandleIORef
      B.hPut geoFileHandle $ ScrB.writePoint vertex newPointId
      return newPointId
  where
  --Check to see if the Vertex has already had a PointId assigned to it.
  -- Will know that by seeing if it has been inserted into the point id map: Map Int (ID.Id ID.PointInt)
  -- If not: get a new PointId, and add it to the point id map: Map Int (ID.Id ID.PointInt). Will increment the PointIsSupply as well.
  -- If yes: get/return the existin PointId
  getSetVertexId :: (Enviro.HasPointIdMap env, Enviro.HasIdSupply env) => Geo.Vertex -> RIO env  PointIdStatus
  getSetVertexId doesThisVertexHaveAnId = do
    pointMapIORef <- view Enviro.pointIdMapL
    pointMap <- readIORef pointMapIORef
    case Map.lookup (H.hash doesThisVertexHaveAnId) pointMap of
      Just pointId -> return $ PointIdAlreadyExisted pointId
      Nothing -> do
        env <- ask
        newPointId <- runRIO env getSetPointId 
        runRIO env $ insertNewPointIntoPointMap  doesThisVertexHaveAnId newPointId
        return $ PointIdDidNotExist newPointId
  
  insertNewPointIntoPointMap :: (Enviro.HasPointIdMap env) => Geo.Vertex -> ID.Id ID.PointInt -> RIO env ()
  insertNewPointIntoPointMap vertex_ pointId = do
    pointMapIORef <- view Enviro.pointIdMapL
    pointMap <- readIORef pointMapIORef
    writeIORef pointMapIORef $ Map.insert (H.hash vertex_ ) pointId pointMap
  
  -- Access the IORef (ID.Id ID.PointInt) in the Environment to:
  -- Get the currently available (ID.Id ID.PointInt)
  -- Increment the IORef (ID.Id ID.PointInt)
  getSetPointId :: (Enviro.HasIdSupply env) => RIO env (ID.Id ID.PointInt)
  getSetPointId = do
    poIntIdSupplyioref <- view Enviro.pointIdSupplyL
    currPointId <- readIORef poIntIdSupplyioref
    writeIORef poIntIdSupplyioref (ID.incr currPointId )
    return currPointId

-- | Process a ['Geo.Vertex'] into a 'PointIdList'. Print any new 'ID.PointId' to .geo file.
-- The [PointIdList] will have at least 3 lines to form a closed polygon such as triangle, square, or irregular shape. The 2nd vertex of the last line will be == 1st vertex of 1st line, to form a closed loop.
--
-- Side effects: Makes changes to the PointId supply and 'Geometry.Vertex.Vertex' map IORefs in 'Enviro.Environment'
-- Returns a Left 'Hex.SafeList3MinError' exception is the vertex list length < 3, as a [ID.Id ID.PointInt] must have at least 3 Vertex to form a closed polygon such as triangle, square, or irregular shape.
--
-- toDo: Should the idea of a list of lines being closed be repesented/enforece with a type such as Closed {closedList :: [a]}
-- For now, will simpley return a closed list, without any type or enforcement.
toPoints :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> RIO env (Either Hex.HasMeshException PointIdList)
toPoints [] = return $ Left $ Hex.SafeList3MinError "length == 0"
toPoints [_] = return $ Left $ Hex.SafeList3MinError "length == 1"
toPoints [_,_] = return $ Left $ Hex.SafeList3MinError "length == 2"
toPoints (x:y:ys) = do
  let
    toointIdList :: [ID.Id ID.PointInt] -> PointIdList
    toointIdList vertex =
      let
        decode :: [ID.Id ID.PointInt] -> PointIdList
        decode (x':y':z':zs) = L.Cons x' y' z' zs  L.Nil
      in
        decode $ reverse vertex
    toPoints' :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> [Geo.Vertex] -> [ID.Id ID.PointInt] -> RIO env (Either Hex.HasMeshException PointIdList)
    --The last vertex, so get an Id, and create/return the safelist.
    toPoints' initVertex (x:[]) workingPoints = do
      --runSimpleApp $ logInfo $ displayShow "in toPoints'"
      env <- ask
      xPointId <- runRIO env $ toPoint x
      case x == initVertex of
        True -> do
          --runSimpleApp $ logInfo "True"
          return $ Right $ toointIdList (xPointId:workingPoints)
        False -> do
          --runSimpleApp $ logInfo "False"
          initId <- runRIO env $ toPoint initVertex
          return $ Right $ toointIdList (initId:xPointId:workingPoints)
    --Not yet the last vertex, so get the Id and continue processing the [vertex]
    toPoints' initVertex (v:vs) workingPoints = do
      env <- ask
      pointId <- runRIO env $ toPoint v
      toPoints' initVertex vs (pointId:workingPoints)
  env <- ask
  runRIO env $ toPoints' x (x:y:ys) []

-- | A 'Utils.List.SafeList3' containing [ID.Id ID.PointInt] for containing a min length of 3 list of Gmsh point Ids.
type PointIdList = L.SafeList3 (ID.Id ID.PointInt) L.NonEmptyID

-}

{-
-------------------------------- Internal tests for non-exported functions ---------------------------
runTests = do
-- create 2 vertex, get their gmsh ids, then create a new line from those ids.
 let
  testCreateLineFromVertexs = TestCase
   (do
      env <- EnvLdr.loadTestEnvironment
      
      point1 <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      point2 <- runRIO env $ Pts.toPoint $ Geo.newVertex  4 5 6
      lineId <- runRIO env $ Line.createLineFromPoints point1 point2
      --runSimpleApp $ logInfo $ displayShow lineId
      assertEqual "create line from 2 point ids" (Gmsh.LineId $ Gmsh.LineInt 1) lineId
   )
 runTestTT testCreateLineFromVertexs

--Load an environment in IO, then call Pts.toPoint to get the PointId for a vertex.
 let
  testGetVertexIdsUsingRIO = TestCase
   (do
      
      
      env <- EnvLdr.loadTestEnvironment
      result <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      assertEqual "get the vector id from an ioref" (Gmsh.PointId $ Gmsh.PointInt 1) result 
   )
 runTestTT testGetVertexIdsUsingRIO


--Load an environment in IO, then call Pts.toPoint to get the PointId for a 2nd vertex.
 let
  testGetVertexIdsUsingRIO2 = TestCase
   (do
      
      
      env <- EnvLdr.loadTestEnvironment
      result1 <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      result <- runRIO env $ Pts.toPoint $ Geo.newVertex  4 5 6
      assertEqual "get the vector id from an ioref" (Gmsh.PointId $ Gmsh.PointInt 2) result 
   )
 runTestTT testGetVertexIdsUsingRIO2


--Load an environment in IO, then insert 2 identical Vertexs to see that it was only inserted once.
 let
  testGetVertexIdsUsingRIO3 = TestCase
   (do
      
      
      env <- EnvLdr.loadTestEnvironment
      result1 <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      result <- runRIO env $ Pts.toPoint $ Geo.newVertex  1 2 3
      assertEqual "get the vector id from an ioref" (Gmsh.PointId $ Gmsh.PointInt 1) result 
   )
 runTestTT testGetVertexIdsUsingRIO3


-}



