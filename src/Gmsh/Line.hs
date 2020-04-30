{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- | Supply a Gmsh Line Id as, 'Gmsh.ID.LineId'.

As in Gmsh, each line is made up of numerous points. HasMesh enforces a minimum of 3 lines to be created from
either ['Geometry.Vertex.Vertex'] or a ['Gmsh.ID.PointId']. The ['Gmsh.ID.LineId'] will be 'Gmsh.Status.Closed'.

import qualified Gmsh.Line as Line or import via Gmsh.Gmsh
-}
module Gmsh.Line({-newLineId,-} createLinesFromPoints, createLinesFromVertex) where

import RIO
--import qualified RIO.Map as Map

import qualified RIO.ByteString as B
import qualified RIO.Text as T
import qualified Geometry.Geometry as Geo
--import qualified Gmsh.ID as ID
import qualified Utils.Environment as Env
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.Environment as Enviro
import qualified Utils.List as L
import qualified Gmsh.Point as Pnt
import qualified Utils.RunExceptions as HexR


-- | A 'Utils.List.SafeList3' containing [Env.Id Env.PointInt].
-- The list will always be 'Gmsh.Status.Closed'
type LineIdSafe3List = L.SafeList3 (Env.Id Env.LineInt) L.NonEmptyID

{-moved to Environment
-- | Creates a 'Env.Id Env.LineInt' that associates 2 'Env.Id Env.PointInt' into a Gmsh line.
-- Will create a new 'Env.Id Env.LineInt' even if the same 2 'Env.Id Env.PointInt' were already used for a 'Env.Id Env.LineInt'
newLineId :: (Enviro.HasIdSupply env) =>   RIO env (Env.Id Env.LineInt)
newLineId = do
  lineIdSupplyIORef <- view Enviro.lineIdSupplyL
  lineIdSupply <- readIORef lineIdSupplyIORef
  writeIORef lineIdSupplyIORef $ Env.incr lineIdSupply
  return lineIdSupply
-}
----------------------------------------------------------------------------------------------------------------------------
-- work to standardize id system
--instance Env.Identifiers LineInt where
  

----------------------------------------------------------------------------------------------------------------------------

--  Create a new Line from 2 gmsh line ids. Called by createLinesFromPoints to create each line in the [line] that it is creating.
createLineFromPoints :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => Env.Id Env.PointInt -> Env.Id Env.PointInt -> RIO env (Env.Id Env.LineInt)
createLineFromPoints pointId1 pointId2 = do
  env <- ask
  handleIORef <- view Enviro.geoFileHandleL
  handle_ <- readIORef handleIORef
  lineId  <- runRIO env Env.getLineId
  --Does not write to handle, unless the hPut call is followed by(not preceded by) the display show.
  --Why is this not the case for writing points in Point.hs
  B.hPut handle_ $ ScrB.writeLine lineId pointId1 pointId2
  runSimpleApp $ logInfo $ displayShow lineId
  return lineId


-- | Generate a 'Gmsh.Status.Closed' 'Gmsh.Env.LineIdSafe3List'. 
createLinesFromVertex :: (Enviro.HasGeoFileHandle env, Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Env.HasScriptWriter env) => T.Text -> [Geo.Vertex] -> RIO env LineIdSafe3List
createLinesFromVertex errMsg vertex = do
  env <- ask 
  points <- runRIO env $ Pnt.toPoints vertex >>= HexR.runEitherRIO errMsg
  createLinesFromPoints points
  
  
-- | Generate a 'Gmsh.Status.Closed' 'Gmsh.Env.LineIdSafe3List'. 
createLinesFromPoints :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => Pnt.PointIdList ->  RIO env LineIdSafe3List 
createLinesFromPoints (L.Cons x y z (x':y':z':z'':zs) _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints y z
  lineyzId <- runRIO env $ createLineFromPoints z x'
  linezx'Id <- runRIO env $ createLineFromPoints x y
  createSafeListOfLinesFromPoints' x (L.Cons x' y' z' (z'':zs) L.Nil) (L.Cons linezx'Id lineyzId linexyId  [] L.Nil)

createLinesFromPoints (L.Cons x y z [x', y',z'] _)  = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  linezx'Id <- runRIO env $ createLineFromPoints z x' 
  createSafeListOfLinesFromPoints' x (L.Cons x' y' z' [] L.Nil)  (L.Cons linezx'Id  lineyzId  linexyId [] L.Nil)
  
  
createLinesFromPoints (L.Cons x y z [y',z'] _)  = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  linezy'Id <- runRIO env $ createLineFromPoints z y' 
  liney'z'Id <- runRIO env $ createLineFromPoints y' z'
  lineClose <- runRIO env $ createLineFromPoints z' x
  return (L.Cons linexyId lineyzId linezy'Id   [liney'z'Id, lineClose] L.Nil)

createLinesFromPoints (L.Cons x y z [y'] _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  linezy'Id <- runRIO env $ createLineFromPoints z y'
  lineClose <- runRIO env $ createLineFromPoints y' x
  return (L.Cons linexyId lineyzId linezy'Id [lineClose] L.Nil)
  

createLinesFromPoints (L.Cons x y z [] _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  lineClose <- runRIO env $ createLineFromPoints z x
  return (L.Cons linexyId lineyzId lineClose [] L.Nil)
  
  

createSafeListOfLinesFromPoints' :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env) => Env.Id Env.PointInt ->  Pnt.PointIdList -> LineIdSafe3List ->  RIO env LineIdSafe3List
createSafeListOfLinesFromPoints' initialPnt (L.Cons x y z (x':y':z':zs) _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  createSafeListOfLinesFromPoints' initialPnt (L.Cons y z x' (y':z':zs) L.Nil) (L.appendSafeList3 linexyId safeWorkingList)
  
  
createSafeListOfLinesFromPoints' initialPnt (L.Cons x y z [y',z'] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  createSafeListOfLinesFromPoints' initialPnt (L.Cons y z y' [z'] L.Nil) (L.appendSafeList3 linexyId safeWorkingList)
  

createSafeListOfLinesFromPoints' initialPnt (L.Cons x y z [y'] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  createSafeListOfLinesFromPoints' initialPnt (L.Cons x y z [y'] L.Nil) (L.appendSafeList3 linexyId safeWorkingList)
  

--the nature of closed PointIdList is: list of 3 should not be possible, perhaps need closed and open, or points should be safe4
--I see I forgot to append the lineClose but no tests fail. Write a test that fails, then fix this.
createSafeListOfLinesFromPoints' initialPnt (L.Cons x y z [] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  lineClose <- runRIO env $ createLineFromPoints z initialPnt
  return $ L.reverseSafeList3 $ L.appendSafeList3 lineClose $ L.appendSafeList3 lineyzId $  L.appendSafeList3 linexyId safeWorkingList
  
  


