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
module Gmsh.Line(newLineId, createLinesFromPoints, {-createLinesFromVertex,-} createLinesFromVertex) where

import RIO
import qualified RIO.Map as Map
import qualified RIO.ByteString as B
import qualified RIO.Text as T

import qualified Geometry.Geometry as Geo
import qualified Gmsh.ID as ID
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.Environment as Enviro
import qualified Utils.List as L
import qualified Gmsh.Point as Pnt
import qualified Utils.RunExceptions as HexR

-- | Creates a 'ID.Id ID.LineInt' that associates 2 'ID.Id ID.PointInt' into a Gmsh line.
-- Will create a new 'ID.Id ID.LineInt' even if the same 2 'ID.Id ID.PointInt' were already used for a 'ID.Id ID.LineInt'
newLineId :: (Enviro.HasIdSupply env) =>   RIO env (ID.Id ID.LineInt)
newLineId = do
  env <- ask
  lineIdSupplyIORef <- view Enviro.lineIdSupplyL
  lineIdSupply <- readIORef lineIdSupplyIORef
  writeIORef lineIdSupplyIORef $ ID.incr lineIdSupply
  return lineIdSupply
  




-- | A 'Utils.List.SafeList3' containing [ID.Id ID.PointInt] for a min length of 3 list of Gmsh point Ids.
type LineIdSafe3List = L.SafeList3 (ID.Id ID.LineInt) L.NonEmptyID



createLinesFromVertex :: (Enviro.HasGeoFileHandle env, Enviro.HasIdSupply env, Enviro.HasPointIdMap env) => T.Text -> [Geo.Vertex] -> RIO env LineIdSafe3List
createLinesFromVertex errMsg vertex = do
  let
    -- Create a ['ID.Id ID.LineInt'] from a ['Geometry.Vertex.Vertex'], which is a [Gmsh lines]
    -- If the ['Geometry.Vertex.Vertex'] has > 3 items, a 'Utils.Exceptions.HasMeshException' will be thrown.
    createLinesFromVertex' :: (Enviro.HasGeoFileHandle env, Enviro.HasIdSupply env, Enviro.HasPointIdMap env) => T.Text -> [Geo.Vertex] -> RIO env [ID.Id ID.LineInt]
    createLinesFromVertex' errMsg vertex = do
      env <- ask
      runRIO env $ Pnt.toPoints vertex >>= HexR.runEitherRIO errMsg >>= createUnsafeListOfLinesFromPoints 
        
  
  env <- ask
  lines <- runRIO env $ createLinesFromVertex' errMsg vertex
  return $ toLineList lines
{-
createLinesFromVertex :: (Enviro.HasGeoFileHandle env, Enviro.HasIdSupply env, Enviro.HasPointIdMap env) => T.Text -> [Geo.Vertex] -> RIO env LineIdSafe3List
createLinesFromVertex errMsg vertex = do
  let
    -- Create a ['ID.Id ID.LineInt'] from a ['Geometry.Vertex.Vertex'], which is a [Gmsh lines]
    -- If the ['Geometry.Vertex.Vertex'] has > 3 items, a 'Utils.Exceptions.HasMeshException' will be thrown.
    createLinesFromVertex' :: (Enviro.HasGeoFileHandle env, Enviro.HasIdSupply env, Enviro.HasPointIdMap env) => T.Text -> [Geo.Vertex] -> RIO env [ID.Id ID.LineInt]
    createLinesFromVertex' errMsg vertex = do
      env <- ask
      lines <- runRIO env $ Pnt.toPoints vertex >>= HexR.runEitherRIO errMsg >>= createUnsafeListOfLinesFromPoints 
      return lines  
  
  env <- ask
  lines <- runRIO env $ createLinesFromVertex' errMsg vertex
  return $ toLineList lines
-}


-- Create a Utils.Line.SafeList3 LineInt from a [ID.Id ID.LineInt]
-- Only call with a [ID.Id ID.LineInt] that is guaranteed to meet the List.SafeList3 criteria, as it is not checked here. In other words, use a [ID.Id ID.LineInt]
-- that got its Gmsh.Points which were generated from Gmsh.Point.toPoints as toPoints it will throw an error if not a Safe PointIdList
toLineList :: [ID.Id ID.LineInt] -> LineIdSafe3List
toLineList (x:y:z:zs) = L.Cons x y z zs  L.Nil

--  Create a new Line from 2 gmsh line ids. Called by createLinesFromPoints to create each line in the [line] that it is creating.
createLineFromPoints :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => ID.Id ID.PointInt -> ID.Id ID.PointInt -> RIO env (ID.Id ID.LineInt)
createLineFromPoints pointId1 pointId2 = do
  env <- ask
  handleIORef <- view Enviro.geoFileHandleL
  handle_ <- readIORef handleIORef
  lineId  <- runRIO env newLineId
  --Does not write to handle, unless the hPut call is followed by(not preceded by) the display show.
  --Why is this not the case for writing points in Point.hs
  B.hPut handle_ $ ScrB.writeLine lineId pointId1 pointId2
  runSimpleApp $ logInfo $ displayShow lineId
  return lineId
{-
toLineList :: [ID.Id ID.LineInt] -> LineIdSafe3List
toLineList (x:y:ys) = L.Cons x y ys  L.Nil

--  Create a new Line from 2 gmsh line ids. Called by createLinesFromPoints to create each line in the [line] that it is creating.
createLineFromPoints :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => ID.Id ID.PointInt -> ID.Id ID.PointInt -> RIO env (ID.Id ID.LineInt)
createLineFromPoints pointId1 pointId2 = do
  env <- ask
  handleIORef <- view Enviro.geoFileHandleL
  handle <- readIORef handleIORef
  lineId  <- runRIO env newLineId
  --Does not write to handle, unless the hPut call is followed by(not preceded by) the display show.
  --Why is this not the case for writing points in Point.hs
  B.hPut handle $ ScrB.writeLine lineId pointId1 pointId2
  runSimpleApp $ logInfo $ displayShow lineId
  return lineId

-}  

-- Create a ['ID.Id ID.LineInt'] from a ['ID.Id ID.PointInt']
-- The ['ID.Id ID.PointInt'] must have at least 3 items, or no line can be generated and so it uses an input of 'Pnt.PointIdList' to ensure this minimum length of 3.
-- The 'Pnt.PointIdList' is created by 'Gmsh.Point.toPoints' which will throw an error if there are > 3 input 'Geometry.Vertex.Vertex', which gives a single entry point
-- to creating a 'Pnt.PointIdList'.
createUnsafeListOfLinesFromPoints :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => Pnt.PointIdList -> RIO env [ID.Id ID.LineInt]
createUnsafeListOfLinesFromPoints (L.Cons x y z zs _) = do
  let
    --Create [line] from [points]. 
    createUnsafeListOfLinesFromPoints' :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => [ID.Id ID.PointInt] -> [ID.Id ID.LineInt] -> RIO env ([ID.Id ID.LineInt])
    createUnsafeListOfLinesFromPoints' (x':x'':[]) workingList = do
      env <- ask
      lastLine <- runRIO env $ createLineFromPoints x' x''
      return $ reverse $ (lastLine:workingList)
    createUnsafeListOfLinesFromPoints' (x':x'':xs) workingList = do
      env <- ask
      currLine <- runRIO env $ createLineFromPoints x' x''
      createUnsafeListOfLinesFromPoints' (x'':xs) (currLine:workingList)
  env <- ask 
  runRIO env $ createUnsafeListOfLinesFromPoints' (x:y:z:zs) [] 
{-
createUnsafeListOfLinesFromPoints :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => Pnt.PointIdList -> RIO env [ID.Id ID.LineInt]
createUnsafeListOfLinesFromPoints (L.Cons x y z zs _) = do
  let
    createUnsafeListOfLinesFromPoints' :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => [ID.Id ID.PointInt] -> [ID.Id ID.LineInt] -> RIO env ([ID.Id ID.LineInt])
    createUnsafeListOfLinesFromPoints' (x':x'':[]) workingList = do
      env <- ask
      lastLine <- runRIO env $ createLineFromPoints x' x''
      return $ reverse $ (lastLine:workingList)
    createUnsafeListOfLinesFromPoints' (x':x'':xs) workingList = do
      env <- ask
      currLine <- runRIO env $ createLineFromPoints x' x''
      createUnsafeListOfLinesFromPoints' (x'':xs) (currLine:workingList)
  env <- ask 
  runRIO env $ createUnsafeListOfLinesFromPoints' (x:y:z:zs) [] 

-}  

-- | Create a [gmsh line ids] which is garanteed to have 3 items.
-- It is a closed loop as the endpoint of the last line is == the initial point of the first line.
createLinesFromPoints :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => Pnt.PointIdList -> RIO env LineIdSafe3List
createLinesFromPoints pointIdList = do
  env <- ask 
  unsafeLines <- runRIO env $ createUnsafeListOfLinesFromPoints pointIdList
  return $ toLineList unsafeLines
  
