{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- | Create a Gmsh Plane Surface

import qualified Gmsh.PlaneSurface as PS
-}
module Gmsh.PlaneSurface(toPlaneSurface) where

import RIO
--import RIO.Text as T
import qualified Utils.List as L
import qualified Utils.Environment as Env

-- Create gmsh plane surface from [curve loops]
toPlaneSurface ::  (Env.HasIdSupply env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => L.CurveIdSafe1List -> RIO env L.PlaneSurfaceSafe1List
toPlaneSurface curveLoopIds = do
  env <- ask
  handleIORef <- view Env.geoFileHandleL
  handle_ <- readIORef handleIORef
  planeSurfaceId <- runRIO env  Env.getPlaneSurfaceId
  planeSurfaceWriter <- view Env.planeSurfaceScriptWriterL

  _ <- liftIO $  planeSurfaceWriter handle_ planeSurfaceId  $ L.evalSafeList1 curveLoopIds
  return $ L.Cons1 planeSurfaceId [] L.Nil1
  
{-
toPlaneSurface ::  (Env.HasIdSupply env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => L.CurveIdSafe1List -> RIO env L.PlaneSurfaceSafe1List
toPlaneSurface curveLoopIds = do
  env <- ask
  handleIORef <- view Env.geoFileHandleL
  handle_ <- readIORef handleIORef
  planeSurfaceId <- runRIO env  Env.getPlaneSurfaceId
  planeSurfaceWriter <- view Env.planeSurfaceScriptWriterL

  _ <- liftIO $  planeSurfaceWriter handle_ planeSurfaceId  $ L.evalSafeList1 curveLoopIds
  return $ L.Cons1 planeSurfaceId [] L.Nil1

-}
