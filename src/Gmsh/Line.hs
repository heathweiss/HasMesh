{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- | Supply a Gmsh Line Id as, 'Gmsh.ID.LineId'.

As in Gmsh, each line is made up of numerous points. HasMesh enforces a minimum of 3 lines to be created from a
Pnt.PointIdList, and therefore is gauranteed not to have any duplicate 'Geometry.Vertex.Vertex'

import qualified Gmsh.Line as Line or import via Gmsh.Gmsh
-}
module Gmsh.Line(toLines) where

import RIO
import qualified Utils.Environment as Env
import qualified Utils.Environment as Enviro
import qualified Utils.List as L
import qualified Gmsh.Point as Pnt

-- | A 'Utils.List.SafeList3' containing [Env.Id Env.PointInt].
type LineIdSafe3List = L.SafeList3 (Env.Id Env.LineInt) L.NonEmptyID

-- Create a new Line from 2 gmsh point ids. Called by toLines to create each line in the [line] that it is creating.
createLineFromPoints :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env, Env.HasScriptWriter env) => Env.Id Env.PointInt -> Env.Id Env.PointInt -> RIO env (Env.Id Env.LineInt)
createLineFromPoints pointId1 pointId2 = do
  env <- ask
  handleIORef <- view Enviro.geoFileHandleL
  handle_ <- readIORef handleIORef
  lineId  <- runRIO env Env.getLineId
  lineWriter <- view Env.lineScriptWriterL
  liftIO $ lineWriter handle_ lineId pointId1 pointId2
  
-- | Generate a 'Gmsh.Env.LineIdSafe3List' from a 'Pnt.PointIdList' 
toLines :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env, Env.HasScriptWriter env) => Pnt.PointIdList ->  RIO env LineIdSafe3List

toLines (L.Cons x y z (x':y':z':z'':zs) _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints y z
  lineyzId <- runRIO env $ createLineFromPoints z x'
  linezx'Id <- runRIO env $ createLineFromPoints x y
  toLinesRecur x (L.Cons x' y' z' (z'':zs) L.Nil) (L.Cons linezx'Id lineyzId linexyId  [] L.Nil)

toLines (L.Cons x y z [x', y',z'] _)  = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  linezx'Id <- runRIO env $ createLineFromPoints z x' 
  toLinesRecur x (L.Cons x' y' z' [] L.Nil)  (L.Cons linezx'Id  lineyzId  linexyId [] L.Nil)
  
  
toLines (L.Cons x y z [y',z'] _)  = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  linezy'Id <- runRIO env $ createLineFromPoints z y' 
  liney'z'Id <- runRIO env $ createLineFromPoints y' z'
  lineClose <- runRIO env $ createLineFromPoints z' x
  return (L.Cons linexyId lineyzId linezy'Id   [liney'z'Id, lineClose] L.Nil)

toLines (L.Cons x y z [y'] _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  linezy'Id <- runRIO env $ createLineFromPoints z y'
  lineClose <- runRIO env $ createLineFromPoints y' x
  return (L.Cons linexyId lineyzId linezy'Id [lineClose] L.Nil)
  

toLines (L.Cons x y z [] _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  lineClose <- runRIO env $ createLineFromPoints z x
  return (L.Cons linexyId lineyzId lineClose [] L.Nil)
  
  

toLinesRecur :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env, Env.HasScriptWriter env) => Env.Id Env.PointInt ->  Pnt.PointIdList -> LineIdSafe3List ->  RIO env LineIdSafe3List

toLinesRecur initialPnt (L.Cons x y z (x':y':z':zs) _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  toLinesRecur initialPnt (L.Cons y z x' (y':z':zs) L.Nil) (L.appendSafeList3 linexyId safeWorkingList)
  
  
toLinesRecur initialPnt (L.Cons x y z [y',z'] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  toLinesRecur initialPnt (L.Cons y z y' [z'] L.Nil) (L.appendSafeList3 linexyId safeWorkingList)
  

toLinesRecur initialPnt (L.Cons x y z [y'] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  toLinesRecur initialPnt (L.Cons x y z [y'] L.Nil) (L.appendSafeList3 linexyId safeWorkingList)
  

toLinesRecur initialPnt (L.Cons x y z [] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  lineClose <- runRIO env $ createLineFromPoints z initialPnt
  return $ L.reverseSafeList3 $ L.appendSafeList3 lineClose $ L.appendSafeList3 lineyzId $  L.appendSafeList3 linexyId safeWorkingList
  
  
  


