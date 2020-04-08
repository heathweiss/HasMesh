{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | Supply functions to support the  'Gmsh.ID.LineId' ADT and associated classes.

import qualified Gmsh.Line as Line
or import via Gmsh.Gmsh
-}
module Gmsh.Line(getLineId, createLineFromPoints) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.ByteString as B

import qualified Geometry.Geometry as Geo
import qualified Gmsh.ID as ID
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.Environment as Enviro

-- | Combination of a 'ID.Id ID.LineInt' and the 2 associated 'ID.Id ID.PointInt'
-- For now. Stick to using just the line id, as can see no point in combining them with the point ids.
-- Might as well use the existing Id system from Gmsh.ID.
--data Line = Line {lineId :: ID.Id ID.LineInt, pointId1 :: ID.Id ID.PointInt, pointId2 :: ID.Id ID.PointInt }

getLineId :: (Enviro.HasLineIdSupply env) =>   RIO env (ID.Id ID.LineInt)
getLineId = do
  env <- ask
  lineIdSupplyIORef <- view Enviro.lineIdSupplyL
  lineIdSupply <- readIORef lineIdSupplyIORef
  writeIORef lineIdSupplyIORef $ ID.incr lineIdSupply
  return lineIdSupply
  
-- | Create a new Line from 2 gmsh line ids.
createLineFromPoints :: (Enviro.HasLineIdSupply env, Enviro.HasGeoFileHandle env) => ID.Id ID.PointInt -> ID.Id ID.PointInt -> RIO env (ID.Id ID.LineInt)
createLineFromPoints (pointId1) (pointId2) = do
  env <- ask
  handleIORef <- view Enviro.geoFileHandleL
  handle <- readIORef handleIORef
  lineId  <- runRIO env getLineId
  --Does not write to handle, unless the hPut call is followed by(not preceded by) the display show.
  --Why is this not the case for writing points in Point.hs
  B.hPut handle $ ScrB.writeLine lineId pointId1 pointId2
  runSimpleApp $ logInfo $ displayShow lineId
  return (lineId)

-- | Create a ['ID.Id ID.LineInt'] from a ['ID.Id ID.PointInt']
-- The ['ID.Id ID.PointInt'] must have at least 2 items, or no line can be generated. Should this be an error.
-- Can I control this with GADT's to give a compile time gaurantee that it has a length of 2.



