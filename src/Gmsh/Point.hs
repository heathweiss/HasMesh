--{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}



{- | Create Gmsh points, as 'Gmsh.ID.PointId' from a 'Geometry.Vertex.Vertex', which is a 3D vertex in the cartesian plane.
   Each vertex will only be assigned a single Id. It a duplicate vertex is encountered, the pre-existing Id will be retrieved.
   
import qualified Gmsh.Point as Pnt  or import via Gmsh.Gmsh
-}
module Gmsh.Point(toPoint, toPoints, PointIdList()) where

import RIO
import qualified Geometry.Geometry as Geo
import qualified Utils.Environment as Env
import qualified Utils.List as L

-- | A 'Utils.List.SafeList3' containing [Env.Id Env.PointInt] for containing a min length of 3 list of Gmsh point Ids.
type PointIdList = L.SafeList3 (Env.Id Env.PointInt) L.NonEmptyID



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
  
-- | Generates a 'PointIdList'.
-- It is guaranteed to have minumum 3 points, with no duplicates.
toPoints ::(Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) =>  L.VertexSafe3List -> RIO env PointIdList
toPoints (L.Cons a b c [] _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  return $ L.Cons aPointId bPointId cPointId [] L.Nil

toPoints (L.Cons a b c [x] _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  return $ L.Cons aPointId bPointId cPointId [xPointId] L.Nil

toPoints (L.Cons a b c [x,y] _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  yPointId <- runRIO env $ toPoint y
  return $ L.Cons aPointId bPointId cPointId [xPointId, yPointId] L.Nil

toPoints (L.Cons a b c [x,y,z] _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  yPointId <- runRIO env $ toPoint y
  zPointId <- runRIO env $ toPoint z
  return $ L.Cons aPointId bPointId cPointId [xPointId, yPointId, zPointId] L.Nil
  
toPoints (L.Cons a b c (x:y:z:zs) _) = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  toPointsRecur (L.Cons x y z zs L.Nil) (L.Cons cPointId bPointId aPointId [] L.Nil)
  
-- Do the recursive calls to process any input into toPoints where the VertexIdList length > 6
toPointsRecur ::(Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) =>
                    L.VertexSafe3List -> PointIdList -> RIO env PointIdList
                    
toPointsRecur (L.Cons a b c [] _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  return $ 
    L.reverseSafeList3 $ L.appendSafeList3 cPointId . L.appendSafeList3 bPointId  $ L.appendSafeList3 aPointId workingList

toPointsRecur (L.Cons a b c [x] _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  return $ 
    L.reverseSafeList3 $ L.appendSafeList3 xPointId . L.appendSafeList3 cPointId . L.appendSafeList3 bPointId  $ L.appendSafeList3 aPointId workingList

toPointsRecur (L.Cons a b c [x,y] _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  yPointId <- runRIO env $ toPoint y
  return $ 
    L.reverseSafeList3 $ L.appendSafeList3 yPointId . L.appendSafeList3 xPointId . L.appendSafeList3 cPointId .
    L.appendSafeList3 bPointId  $ L.appendSafeList3 aPointId workingList

toPointsRecur (L.Cons a b c [x,y,z] _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  bPointId <- runRIO env $ toPoint b
  cPointId <- runRIO env $ toPoint c
  xPointId <- runRIO env $ toPoint x
  yPointId <- runRIO env $ toPoint y
  zPointId <- runRIO env $ toPoint z
  return $ 
    L.reverseSafeList3 $ L.appendSafeList3 zPointId . L.appendSafeList3 yPointId . L.appendSafeList3 xPointId .
                         L.appendSafeList3 cPointId . L.appendSafeList3 bPointId  $ L.appendSafeList3 aPointId workingList
                         

toPointsRecur (L.Cons a b c (x:y:z:zs) _) workingList = do
  env <- ask
  aPointId <- runRIO env $ toPoint a
  toPointsRecur (L.Cons b c x (y:z:zs) L.Nil) $ L.appendSafeList3 aPointId workingList
  
  


  
 
