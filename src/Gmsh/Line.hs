{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | Supply functions to support the  'Gmsh.ID.LineId' ADT and associated classes.

"Points" in this module refers to 'Gmsh.Id ID.PointInt', which is the Gmsh Id associated with a 'Geometry.Vertex'

import qualified Gmsh.Line as Line
or import via Gmsh.Gmsh
-}
module Gmsh.Line(getLineId, createLineFromPoints, createLinesFromPoints) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.ByteString as B

import qualified Geometry.Geometry as Geo
import qualified Gmsh.ID as ID
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.Environment as Enviro
import qualified Utils.List as L
import qualified Gmsh.Point as Pnt  

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
-- The ['ID.Id ID.PointInt'] must have at least 2 items, or no line can be generated. Uses an input of 'Pnt.PointIdList' to enforce min length of 2.
createLinesFromPoints :: (Enviro.HasLineIdSupply env, Enviro.HasGeoFileHandle env) => Pnt.PointIdList -> RIO env [ID.Id ID.LineInt]
createLinesFromPoints (L.Cons x y ys _) = do
--createLinesFromPoints _ = do
  let
    createLinesFromPoints' :: (Enviro.HasLineIdSupply env, Enviro.HasGeoFileHandle env) => [ID.Id ID.PointInt] -> [ID.Id ID.LineInt] -> RIO env ([ID.Id ID.LineInt])
    createLinesFromPoints' (x':x'':[]) workingList = do
      env <- ask
      lastLine <- runRIO env $ createLineFromPoints x' x''
      return $ reverse $ (lastLine:workingList)
    createLinesFromPoints' (x':x'':xs) workingList = do
      env <- ask
      currLine <- runRIO env $ createLineFromPoints x' x''
      createLinesFromPoints' (x'':xs) (currLine:workingList)
  env <- ask 
  runRIO env $ createLinesFromPoints' (x:y:ys) []
  --return _


{-
createLinesFromPoints :: (Enviro.HasLineIdSupply env, Enviro.HasGeoFileHandle env) => Pnt.PointIdList -> RIO env [ID.Id ID.LineInt]
--createLinesFromPoints (L.Cons x y ys _) = do
createLinesFromPoints _ = do
  let
    createLinesFromPoints' :: (Enviro.HasLineIdSupply env, Enviro.HasGeoFileHandle env) => [ID.Id ID.PointInt] -> [ID.Id ID.LineInt] -> RIO env [ID.Id ID.LineInt]
    createLinesFromPoints' (x:x':[]) workingList = do
      reverse workingLIst
  in
    env <- ask 
    runRIO env $ createLinesFromPoints' (x:y:ys) []

-}
