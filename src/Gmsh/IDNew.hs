{-# LANGUAGE NoImplicitPrelude #-}

{- |
Functions that combine 'Gmsh.ID' and 'Utils.Environment' together, as enviro already import id, and so would form a cycle.

import qualified Gmsh.IDNew as IDNew

or

import qualified Gmsh.Gmsh as Gmsh
-}
module Gmsh.IDNew({-PointIdStatus(..), newPointId-}) where

import RIO
import qualified RIO.Map as Map
import qualified Data.Hashable as H

import qualified Gmsh.ID as ID
import qualified Utils.Environment as Enviro
import qualified Geometry.Geometry as Geo
import qualified RIO.ByteString as B
import qualified Gmsh.ToScript.BuiltIn as ScrB


data PointIdStatus = PointIdAlreadyExisted (ID.Id ID.PointInt) |  PointIdDidNotExist (ID.Id ID.PointInt) deriving (Eq, Show)


class NewID a where
  newId :: (Enviro.HasPointIdMap env, Enviro.HasIdSupply env) => RIO env (ID.Id a)
    


--instance NewID ID.PointInt where
newPointId :: (Enviro.HasPointIdMap env, Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => Geo.Vertex -> RIO env  (ID.Id ID.PointInt)


newPointId vertex = do
  
    let
        getSetVertexId :: (Enviro.HasPointIdMap env, Enviro.HasIdSupply env) => Geo.Vertex -> RIO env  PointIdStatus
        getSetVertexId doesThisVertexHaveAnId = do
          let
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
              
          pointMapIORef <- view Enviro.pointIdMapL
          pointMap <- readIORef pointMapIORef
          case Map.lookup (H.hash doesThisVertexHaveAnId) pointMap of
            
            Just pointId -> return $ PointIdAlreadyExisted pointId
            Nothing -> do
              env <- ask
              newPointId' <- runRIO env getSetPointId
              
              runRIO env $ insertNewPointIntoPointMap  doesThisVertexHaveAnId newPointId'
              return $ PointIdDidNotExist newPointId'
              
          
    env <- ask
    pointIdStatus <- runRIO env $ getSetVertexId vertex
    case pointIdStatus of
      PointIdAlreadyExisted preExistingPointId -> return preExistingPointId
      PointIdDidNotExist newPointId' -> do
        geoFileHandleIORef <- view Enviro.geoFileHandleL
        geoFileHandle <- readIORef geoFileHandleIORef
        
        B.hPut geoFileHandle $ ScrB.writePoint vertex newPointId'
        return newPointId'
        
    
    

