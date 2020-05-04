{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | Supply functions to support the  'Gmsh.ID.LineId' ADT and associated classes.

"Points" in this module refers to 'Gmsh.Id ID.PointInt', which is the Gmsh Id associated with a 'Geometry.Vertex'

import qualified Gmsh.CurveLoop as CL
or import via Gmsh.Gmsh
-}
module Gmsh.CurveLoop(toCurveLoop) where

import RIO
import RIO.Text as T
import qualified Utils.List as L
import qualified Utils.Environment as Env

toCurveLoop :: (Env.HasIdSupply env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => L.LineIdSafe3List -> RIO env L.CurveIdSafe1List
toCurveLoop lineIds = do
  env <- ask
  handleIORef <- view Env.geoFileHandleL
  handle_ <- readIORef handleIORef
  curveLoopId <- runRIO env  Env.getCurveLoopId
  curveLoopWriter <- view Env.curveLoopScriptWriterL
  _ <- liftIO $  curveLoopWriter handle_ curveLoopId  $ L.evalSafeList3 lineIds
  
  return $ L.Cons1 curveLoopId [] L.Nil1
  
  
  
  
  
  


