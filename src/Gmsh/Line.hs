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
import qualified Gmsh.Point as Pnt
import qualified List.Base as LB
import qualified List.Safe3 as L3

-- | A 'Utils.List.SafeList3' containing [Env.Id Env.PointInt].
--type LineIdSafe3List = L3.SafeList3 (Env.Id Env.LineInt) LB.NonEmptyID

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
toLines :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env, Env.HasScriptWriter env) => Pnt.PointIdSafe3List ->  RIO env L3.LineIdSafe3List
toLines (L3.Cons x y z (x':y':z':zs) _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y 
  lineyzId <- runRIO env $ createLineFromPoints y z 
  linezx'Id <- runRIO env $ createLineFromPoints z x'
  toLinesRecur x (L3.Cons x' y' z' zs L3.Nil) (L3.Cons linezx'Id lineyzId linexyId  [] L3.Nil)

  
toLines (L3.Cons x y z [y',z'] _)  = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  linezy'Id <- runRIO env $ createLineFromPoints z y' 
  liney'z'Id <- runRIO env $ createLineFromPoints y' z'
  lineClose <- runRIO env $ createLineFromPoints z' x
  return (L3.Cons linexyId lineyzId linezy'Id   [liney'z'Id, lineClose] L3.Nil)

toLines (L3.Cons x y z [y'] _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  linezy'Id <- runRIO env $ createLineFromPoints z y'
  lineClose <- runRIO env $ createLineFromPoints y' x
  return (L3.Cons linexyId lineyzId linezy'Id [lineClose] L3.Nil)

toLines (L3.Cons x y z [] _) = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  lineClose <- runRIO env $ createLineFromPoints z x
  return (L3.Cons linexyId lineyzId lineClose [] L3.Nil)
  

  
--Recursive call to handle lists > 6.
toLinesRecur :: (Enviro.HasIdSupply env, Enviro.HasGeoFileHandle env, Env.HasScriptWriter env) => Env.Id Env.PointInt ->  Pnt.PointIdSafe3List -> L3.LineIdSafe3List ->  RIO env L3.LineIdSafe3List
toLinesRecur initialPnt (L3.Cons x y z [y',z'] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  toLinesRecur initialPnt (L3.Cons y z y' [z'] L3.Nil) (L3.appendSafeList3 linexyId safeWorkingList)

toLinesRecur initialPnt (L3.Cons x y z [y'] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  toLinesRecur initialPnt (L3.Cons y z y' [] L3.Nil) (L3.appendSafeList3 linexyId safeWorkingList)

toLinesRecur initialPnt (L3.Cons x y z [] _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  lineyzId <- runRIO env $ createLineFromPoints y z
  lineClose <- runRIO env $ createLineFromPoints z initialPnt
  return $ L3.reverseSafeList3 $ L3.appendSafeList3 lineClose $ L3.appendSafeList3 lineyzId $  L3.appendSafeList3 linexyId safeWorkingList
  
toLinesRecur initialPnt (L3.Cons x y z (x':y':zs) _) safeWorkingList = do
  env <- ask
  linexyId <- runRIO env $ createLineFromPoints x y
  toLinesRecur initialPnt (L3.Cons y z x' (y':zs) L3.Nil) (L3.appendSafeList3 linexyId safeWorkingList)


