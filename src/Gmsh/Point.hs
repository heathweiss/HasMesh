{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Supply functions to support the  'Gmsh.ID.PointId' ADT and associated classes.

import qualified Gmsh.Point as Pnt  
-}
module Gmsh.Point(toPoint, toPoints) where

import RIO
import qualified Data.Hashable as H
import qualified RIO.Map as Map
import qualified RIO.ByteString as B

import qualified Utils.Environment as Enviro
import qualified Geometry.Geometry as Geo
import qualified Gmsh.ID as ID
--import qualified Scripting.Scripting as Script
import qualified Gmsh.ToScript.BuiltIn as ScrB


-- | Get the 'ID.PointId' for a 'Geometry.Vertex.Vertex'
--
-- Should this be exported, or kept internal for use with the 'toPoints', which hanlles a ['Geo.Vertex']
-- toDo: need add a handle to 'Enviro.Environment' so that a new point will be written to file.
toPoint :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> RIO env (ID.Id ID.PointInt)
toPoint vertex = do
  pointMapIORef <- view Enviro.env_pointIdMapL
  pointMap <- readIORef pointMapIORef
  let
    hashedVertex = H.hash vertex
  case Map.lookup hashedVertex pointMap of
    Just val -> return val
    Nothing -> do
      poIntIdSupplyioref <- view Enviro.env_pointIdSupplyL
      currPointId <- readIORef poIntIdSupplyioref
      geoFileHandleIORef <- view Enviro.env_geoFileHandleL
      geoFileHandle <- readIORef geoFileHandleIORef
      B.hPut geoFileHandle $ ScrB.writePoint vertex currPointId
      writeIORef pointMapIORef $ Map.insert hashedVertex currPointId pointMap
      writeIORef poIntIdSupplyioref (ID.incr currPointId )
      return currPointId
{-
toPoint :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> RIO env (ID.Id Int)
toPoint vertex = do
  pointMapIORef <- view Enviro.env_pointIdMapL
  pointMap <- readIORef pointMapIORef
  let
    hashedVertex = H.hash vertex
  case Map.lookup hashedVertex pointMap of
    Just val -> return val
    Nothing -> do
      poIntIdSupplyioref <- view Enviro.env_pointIdSupplyL
      currPointId <- readIORef poIntIdSupplyioref
      geoFileHandleIORef <- view Enviro.env_geoFileHandleL
      geoFileHandle <- readIORef geoFileHandleIORef
      B.hPut geoFileHandle $ ScrB.writePoint vertex currPointId
      writeIORef pointMapIORef $ Map.insert hashedVertex currPointId pointMap
      writeIORef poIntIdSupplyioref (ID.incr currPointId )
      return currPointId

toPoint :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> RIO env ID.PointId
toPoint vertex = do
  pointMapIORef <- view Enviro.env_pointIdMapL
  pointMap <- readIORef pointMapIORef
  let
    hashedVertex = H.hash vertex
  case Map.lookup hashedVertex pointMap of
    Just val -> return val
    Nothing -> do
      poIntIdSupplyioref <- view Enviro.env_pointIdSupplyL
      currPointId <- readIORef poIntIdSupplyioref
      geoFileHandleIORef <- view Enviro.env_geoFileHandleL
      geoFileHandle <- readIORef geoFileHandleIORef
      B.hPut geoFileHandle $ Script.writePoint vertex currPointId
      writeIORef pointMapIORef $ Map.insert hashedVertex currPointId pointMap
      writeIORef poIntIdSupplyioref (ID.incr currPointId )
      return currPointId
-}

-- | Process a ['Geo.Vertex'] into a ['ID.PointId']. Print any new 'ID.PointId' to .geo file.
--
-- Side effects: Makes changes to the IORefs in 'Enviro.Environment'
--
-- toDo: -- toDo: need add a handle to 'Enviro.Environment' so that a new point will be written to file.
-- Then add the printing to 'toPoint'
toPoints :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> RIO env [(ID.Id ID.PointInt)]
toPoints [] = return []
toPoints vertexs = do
  let
    toPoints' :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> [ID.Id ID.PointInt] -> RIO env [(ID.Id ID.PointInt)]
    toPoints' [] workingPoints = do
      return $ reverse workingPoints
    toPoints' (v:vs) workingPoints = do
      env <- ask
      pointId <- runRIO env $ toPoint v
      toPoints' vs (pointId:workingPoints)
  env <- ask
  runRIO env $ toPoints' vertexs []
 
{-
toPoints :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> RIO env [(ID.Id Int)]
toPoints [] = return []
toPoints vertexs = do
  let
    toPoints' :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> [(ID.Id Int)] -> RIO env [(ID.Id Int)]
    toPoints' [] workingPoints = do
      return $ reverse workingPoints
    toPoints' (v:vs) workingPoints = do
      env <- ask
      pointId <- runRIO env $ toPoint v
      toPoints' vs (pointId:workingPoints)
  env <- ask
  runRIO env $ toPoints' vertexs []

toPoints :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> RIO env [ID.PointId]
toPoints [] = return []
toPoints vertexs = do
  let
    toPoints' :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => [Geo.Vertex] -> [ID.PointId] -> RIO env [ID.PointId]
    toPoints' [] workingPoints = do
      return $ reverse workingPoints
    toPoints' (v:vs) workingPoints = do
      env <- ask
      pointId <- runRIO env $ toPoint v
      toPoints' vs (pointId:workingPoints)
  env <- ask
  runRIO env $ toPoints' vertexs []

-}
