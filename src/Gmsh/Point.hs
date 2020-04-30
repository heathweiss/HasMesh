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
import qualified Geometry.Geometry as Geo
import qualified Utils.Environment as Env
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR

-- Indicates if the returned Env.Id Env.PointInt is a new Id, or it already exsisted.
--data PointIdStatus = PointIdAlreadyExisted (Env.Id Env.PointInt) |  PointIdDidNotExist (Env.Id Env.PointInt)


-- | A 'Utils.List.SafeList3' containing [Env.Id Env.PointInt] for containing a min length of 3 list of Gmsh point Ids.
type PointIdList = L.SafeList3 (Env.Id Env.PointInt) L.NonEmptyID

-- Get the associated 'Env.PointId'.
-- If the vertex does not already have an 'Env.PointId', will create a new one, and write it to the .geo file handle.
toPoint :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => Geo.Vertex -> RIO env (Env.Id Env.PointInt)
toPoint vertex  = do
  env <- ask
  geoFileHandleIORef <- view Env.geoFileHandleL
  geoFileHandle <- readIORef geoFileHandleIORef

  possiblyNewPointId <- runRIO env $ Env.getPointId vertex
  writePointScript <- view Env.pntScriptWriterL
  liftIO $ writePointScript geoFileHandle possiblyNewPointId vertex
  
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
-- Makes changes to the 'Gmsh.Env.PointId' supply and 'Geometry.Vertex.Vertex' map IORefs, both of which are in 'Env.Environment'
--
-- Prints new 'Env.PointId' to .geo file.
toPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => [Geo.Vertex] -> RIO env (Either Hex.HasMeshException PointIdList)
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
  runRIO env $ toPointsRecur initialVertex vTail reversedInitialSafeWorkingList

--After all pattern matches up to 3 places are handled by toPoints, do the recursive processing of the [Geo.Vertex]
toPointsRecur :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => Geo.Vertex -> [Geo.Vertex] -> PointIdList -> RIO env (Either Hex.HasMeshException PointIdList)
toPointsRecur initVertex [] reversedSafe3WorkingList = do
      env <- ask
      initPointId <- runRIO env $ toPoint initVertex
      if initPointId == L.safeHead3 reversedSafe3WorkingList then --build working list in reverse so append can be used till final iteration.
        return $ Left $ Hex.PointIdSafe3ListIsClosed "PointIdList is closed"
      else 
        return $ Right $ L.reverseSafeList3 reversedSafe3WorkingList
        
toPointsRecur initVertex [x'] reversedSafe3WorkingList = do
      env <- ask
      xPointId <- runRIO env $ toPoint x'
      if x' == initVertex then
        return $ Left $ Hex.PointIdSafe3ListIsClosed "PointIdList is closed"
      else 
        return $ Right $ L.reverseSafeList3 $ L.appendSafeList3 xPointId reversedSafe3WorkingList

    --Not yet the last vertex, so get the Id and continue processing the [vertex]
toPointsRecur initVertex (v:vs) reversedSafe3WorkingList = do
      env <- ask
      vPointId <- runRIO env $ toPoint v
      toPointsRecur initVertex vs $ L.appendSafeList3 vPointId reversedSafe3WorkingList
      



      
      

{-
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

import qualified RIO.ByteString as B
import qualified Geometry.Geometry as Geo
import qualified Utils.Environment as Env
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR

-- Indicates if the returned Env.Id Env.PointInt is a new Id, or it already exsisted.
--data PointIdStatus = PointIdAlreadyExisted (Env.Id Env.PointInt) |  PointIdDidNotExist (Env.Id Env.PointInt)


-- | A 'Utils.List.SafeList3' containing [Env.Id Env.PointInt] for containing a min length of 3 list of Gmsh point Ids.
type PointIdList = L.SafeList3 (Env.Id Env.PointInt) L.NonEmptyID

-- Get the associated 'Env.PointId'.
-- If the vertex does not already have an 'Env.PointId', will create a new one, and write it to the .geo file handle.
toPoint :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env) => Geo.Vertex -> RIO env (Env.Id Env.PointInt)
toPoint vertex  = do
  env <- ask
  --pointIdStatus <- runRIO env $ getSetVertexId vertex
  possiblyNewPointId <- runRIO env $ Env.getPointId vertex
  case possiblyNewPointId of
    Env.PointIdAlreadyExisted preExistingPointId -> return preExistingPointId
    Env.PointIdDidNotExist newPointId -> do
      geoFileHandleIORef <- view Env.geoFileHandleL
      geoFileHandle <- readIORef geoFileHandleIORef
      B.hPut geoFileHandle $ ScrB.writePoint vertex newPointId
      return newPointId

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
-- Makes changes to the 'Gmsh.Env.PointId' supply and 'Geometry.Vertex.Vertex' map IORefs, both of which are in 'Env.Environment'
--
-- Prints new 'Env.PointId' to .geo file.
toPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env) => [Geo.Vertex] -> RIO env (Either Hex.HasMeshException PointIdList)
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

toPoints' :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env) => Geo.Vertex -> [Geo.Vertex] -> PointIdList -> RIO env (Either Hex.HasMeshException PointIdList)
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

-}




