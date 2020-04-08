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


-- | Associate a 'Geometry.Vertex.Vertex' with a 'ID.PointId', which is the Id used by Gmsh in scripting.
-- If the vertex already has an Id, just return the Id, otherwise create a new Id, and write it to a .geo file via the env handle.

toPoint :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> RIO env (ID.Id ID.PointInt)
toPoint vertex = do
  pointMapIORef <- view Enviro.pointIdMapL
  pointMap <- readIORef pointMapIORef
  let
    hashedVertex = H.hash vertex
  case Map.lookup hashedVertex pointMap of
    Just val -> return val
    Nothing -> do
      poIntIdSupplyioref <- view Enviro.pointIdSupplyL
      currPointId <- readIORef poIntIdSupplyioref
      geoFileHandleIORef <- view Enviro.geoFileHandleL
      geoFileHandle <- readIORef geoFileHandleIORef
      B.hPut geoFileHandle $ ScrB.writePoint vertex currPointId
      writeIORef pointMapIORef $ Map.insert hashedVertex currPointId pointMap
      writeIORef poIntIdSupplyioref (ID.incr currPointId )
      return currPointId

-- | Process a ['Geo.Vertex'] into a ['ID.PointId']. Print any new 'ID.PointId' to .geo file.
--
-- Side effects: Makes changes to the PointId supply and vertex map IORefs in 'Enviro.Environment'
--
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
 


