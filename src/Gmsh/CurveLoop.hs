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
import qualified Utils.Environment as Env
import qualified List.Safe3 as L3
import qualified List.Safe1 as L1

-- | Create a sgmh Curve Loop from a list of Gmsh lines.
toCurveLoop :: (Env.HasIdSupply env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => L3.LineIdSafe3List -> RIO env L1.CurveIdSafe1List
toCurveLoop lineIds = do
  env <- ask
  handleIORef <- view Env.geoFileHandleL
  handle_ <- readIORef handleIORef
  curveLoopId <- runRIO env  Env.getCurveLoopId
  curveLoopWriter <- view Env.curveLoopScriptWriterL
  _ <- liftIO $  curveLoopWriter handle_ curveLoopId  $ L3.evalSafeList3 lineIds
  
  return $ L1.Cons curveLoopId [] L1.Nil
  
  
  
  
  
  


