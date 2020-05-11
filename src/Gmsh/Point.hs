--{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}



{- | Create Gmsh points, as 'Gmsh.ID.PointId' from a 'Geometry.Vertex.Vertex', which is a 3D vertex in the cartesian plane.
   Each vertex will only be assigned a single Id. It a duplicate vertex is encountered, the pre-existing Id will be retrieved.
   
import qualified Gmsh.Point as Pnt  or import via Gmsh.Gmsh
-}
module Gmsh.Point(toPoint, toPoints, L3.PointIdSafe3List()) where

import RIO
import qualified Geometry.Geometry as Geo
import qualified Utils.Environment as Env
import qualified Utils.List as L
import qualified List.Safe3 as L3

-- | A 'Utils.List.SafeList3' containing [Env.Id Env.PointInt] for containing a min length of 3 list of Gmsh point Ids.
--This one gets deleted in favor of the following 1.
--type PointIdList = L.SafeList3 (Env.Id Env.PointInt) L.NonEmptyID
--This is the one that should be used.
--type PointIdSafe3List = SafeList3 (Env.Id Env.PointInt) LB.NonEmptyID



-- Get the associated 'Env.PointId'.
-- If the vertex does not already have an 'Env.PointId', will create a new one, and write it to the .geo file handle.
-- If vertex is a duplicate of a previous vertex, just return the pre-existing ID.
toPoint :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => Geo.Vertex -> RIO env (Env.Id Env.PointInt)
toPoint vertex  = do
  env <- ask
  geoFileHandleIORef <- view Env.geoFileHandleL
  geoFileHandle <- readIORef geoFileHandleIORef

  possiblyNewPointId <- runRIO env $ Env.getPointId vertex
  writePointScript <- view Env.pntScriptWriterL
  liftIO $ writePointScript geoFileHandle possiblyNewPointId vertex
  
-- | Generates a 'L.PointIdSafe3List'.
-- It is guaranteed to have minumum 3 points, with no duplicates.
toPoints ::(Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) =>  L3.VertexSafe3List -> RIO env L3.PointIdSafe3List
toPoints (L3.Cons a b c [] _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  return $ L3.Cons aPointId bPointId cPointId [] L3.Nil

toPoints (L3.Cons a b c [x] _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  return $ L3.Cons aPointId bPointId cPointId [xPointId] L3.Nil

toPoints (L3.Cons a b c [x,y] _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  yPointId <- runRIO env $ toPoint y
  return $ L3.Cons aPointId bPointId cPointId [xPointId, yPointId] L3.Nil

toPoints (L3.Cons a b c [x,y,z] _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  yPointId <- runRIO env $ toPoint y
  zPointId <- runRIO env $ toPoint z
  return $ L3.Cons aPointId bPointId cPointId [xPointId, yPointId, zPointId] L3.Nil
  
toPoints (L3.Cons a b c (x:y:z:zs) _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  toPointsRecur (L3.Cons x y z zs L3.Nil) (L3.Cons cPointId bPointId aPointId [] L3.Nil)
  
-- Do the recursive calls to process any input into toPoints where the VertexIdList length > 6
toPointsRecur ::(Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) =>
                    L3.VertexSafe3List -> L3.PointIdSafe3List -> RIO env L3.PointIdSafe3List
                    
toPointsRecur (L3.Cons a b c [] _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  return $ 
    L3.reverseSafeList3 $ L3.appendSafeList3 cPointId . L3.appendSafeList3 bPointId  $ L3.appendSafeList3 aPointId workingList

toPointsRecur (L3.Cons a b c [x] _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  return $ 
    L3.reverseSafeList3 $ L3.appendSafeList3 xPointId . L3.appendSafeList3 cPointId . L3.appendSafeList3 bPointId  $ L3.appendSafeList3 aPointId workingList

toPointsRecur (L3.Cons a b c [x,y] _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  yPointId <- runRIO env $ toPoint y
  return $ 
    L3.reverseSafeList3 $ L3.appendSafeList3 yPointId . L3.appendSafeList3 xPointId . L3.appendSafeList3 cPointId .
    L3.appendSafeList3 bPointId  $ L3.appendSafeList3 aPointId workingList

toPointsRecur (L3.Cons a b c [x,y,z] _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  yPointId <- runRIO env $ toPoint y
  zPointId <- runRIO env $ toPoint z
  return $ 
    L3.reverseSafeList3 $ L3.appendSafeList3 zPointId . L3.appendSafeList3 yPointId . L3.appendSafeList3 xPointId .
                         L3.appendSafeList3 cPointId . L3.appendSafeList3 bPointId  $ L3.appendSafeList3 aPointId workingList
                         

toPointsRecur (L3.Cons a b c (x:y:z:zs) _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  toPointsRecur (L3.Cons b c x (y:z:zs) L3.Nil) $ L3.appendSafeList3 aPointId workingList
  
  


  
 
