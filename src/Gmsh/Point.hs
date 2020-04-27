{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}



{- | Create Gmsh ponts, as 'Gmsh.ID.PointId' from 'Geometry.Vertex.Vertex', which is a 3D vertex in the cartesian plane.
   Each unique vertex will only be assigned a single Id. It the same vertex is encountered, the pre-existing Id will be retrieved.
   Otherwise, it is the first time the vertex has occurrred, and a new Id will be generated.

import qualified Gmsh.Point as Pnt  or import via Gmsh.Gmsh
-}
module Gmsh.Point(toPoint, toPoints, PointIdList(), {-uncomment for internal tests in test/PointTest toPoint, toPoints'-}) where

import RIO
import qualified Data.Hashable as H
import qualified RIO.Map as Map
import qualified RIO.ByteString as B

import qualified Utils.Environment as Enviro
import qualified Geometry.Geometry as Geo
import qualified Gmsh.ID as ID
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR

-- Indicates if the returned ID.Id ID.PointInt is a new Id, or it already exsisted.
data PointIdStatus = PointIdAlreadyExisted (ID.Id ID.PointInt) |  PointIdDidNotExist (ID.Id ID.PointInt)

-- | A 'Utils.List.SafeList3' containing [ID.Id ID.PointInt] for containing a min length of 3 list of Gmsh point Ids.
type PointIdList = L.SafeList3 (ID.Id ID.PointInt) L.NonEmptyID

-- Get the associated 'ID.PointId'.
-- If the vertex does not already have an 'ID.PointId', will create a new one, and write it to the .geo file handle.
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


{-
Should the 'Gmsh.Status.Open' status be enforce.
 -}
-- | Generates an 'Gmsh.Status.Open' 'PointIdList'.
-- If it is not open, then a 'PointIdList' with length == 3, will create ill formed polygon if 1st and last vertex are equal. 
-- The 'Gmsh.Status.Open' state is achived when a 'Gmsh.Line.LineIdSafe3List' if created from the 'PointIdList'
-- Returns a Left 'Hex.SafeList3MinError' exception is the vertex is not 'Gmsh.Status.Open' 
--
-- Side effects:
--
-- Makes changes to the 'Gmsh.ID.PointId' supply and 'Geometry.Vertex.Vertex' map IORefs, both of which are in 'Enviro.Environment'
--
-- Prints new 'ID.PointId' to .geo file.
toPoints :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> RIO env (Either Hex.HasMeshException PointIdList)
toPoints [] = return $ Left $ Hex.SafeList3MinError "length == 0"
toPoints [_] = return $ Left $ Hex.SafeList3MinError "length == 1"
toPoints [_,_] = return $ Left $ Hex.SafeList3MinError "length == 2"
toPoints [x,y,z] = 
  if x == z then return $ Left $ Hex.PointIdSafe3ListIsClosed "PointIdList is closed"
  else
    do
      env <- ask
      xPointId <- runRIO env $ toPoint x
      yPointId <- runRIO env $ toPoint y
      zPointId <- runRIO env $ toPoint z
      return  (L.toSafeList3 [xPointId,yPointId,zPointId])

toPoints (initialVertex:y:z:vTail) = do
  env <- ask
  xPointId <- runRIO env $ toPoint initialVertex
  yPointId <- runRIO env $ toPoint y
  zPointId <- runRIO env $ toPoint z
  reversedInitialSafeWorkingList <- HexR.runEitherRIO "initialSafeList" (L.toSafeList3 [zPointId,yPointId,xPointId])
  runRIO env $ toPoints' initialVertex vTail reversedInitialSafeWorkingList

toPoints' :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> [Geo.Vertex] -> PointIdList -> RIO env (Either Hex.HasMeshException PointIdList)
toPoints' initVertex [] reversedSafe3WorkingList = do
      env <- ask
      initPointId <- runRIO env $ toPoint initVertex
      if initPointId == L.safeHead3 reversedSafe3WorkingList then --build working list in reverse so append can be used till final iteration.
        return $ Left $ Hex.PointIdSafe3ListIsClosed "PointIdList is closed"
      else 
        return $ Right $ L.reverseSafeList3 reversedSafe3WorkingList
        
toPoints' initVertex [x'] reversedSafe3WorkingList = do
      env <- ask
      xPointId <- runRIO env $ toPoint x'
      if x' == initVertex then
        return $ Left $ Hex.PointIdSafe3ListIsClosed "PointIdList is closed"
      else 
        return $ Right $ L.reverseSafeList3 $ L.appendSafeList3 xPointId reversedSafe3WorkingList

    --Not yet the last vertex, so get the Id and continue processing the [vertex]
toPoints' initVertex (v:vs) reversedSafe3WorkingList = do
      env <- ask
      vPointId <- runRIO env $ toPoint v
      toPoints' initVertex vs $ L.appendSafeList3 vPointId reversedSafe3WorkingList





