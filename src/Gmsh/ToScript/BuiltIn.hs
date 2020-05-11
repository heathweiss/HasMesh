{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{- |
Generate Gmsh script that is common to the builtin cad engine and open cascade, such as writing comments.

import qualified Gmsh.ToScript.BuiltIn as ScrB
or as part of the Gmsh import module
import qualified Gmsh.Gmsh as Gmsh
-}
module Gmsh.ToScript.BuiltIn(genPointScript, genCurveLoopScript, genLineScript, genPlaneSurfaceScript,
                             writeLC, writeLC1, writeLC2, writeLC3,
                             lineWriter, nullLineWriter,  
                             nullPointWriter, pointWriter,
                             nullCurveLoopWriter, curveLoopWriter,
                             nullPlaneSurfaceWriter, planeSurfaceWriter) where




import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Data.String.Interpolate ( i )
import qualified Geometry.Vertex as V
import qualified Geometry.Geometry as Geo
import qualified Utils.Environment as Env
import qualified Utils.List as L

-- | Output a gmsh point.
genPointScript :: Geo.Vertex -> Env.Id Env.PointInt -> B.ByteString
genPointScript (Geo.Vertex' x y z) (Env.PointId (Env.PointInt' id)) =
  [i|\nPoint(#{id}) = {#{x},#{y},#{z},lc};|] :: B.ByteString
{-
writePoint :: Geo.Vertex -> Env.Id Int -> B.ByteString
writePoint (Geo.Vertex' x y z) (Env.PointId id) =
  [i|\nPoint(#{id}) = {#{x},#{y},#{z},lc};|] :: B.ByteString

before GADT
writePoint :: Geo.Vertex -> Gmsh.PointId -> B.ByteString
writePoint (Geo.Vertex' x y z) (Gmsh.PointId id) =
  [i|Point(#{id}) = {#{x},#{y},#{z},lc};|] :: B.ByteString
-}

-- | Write the lc var that Gmsh tutorials use to set the mesh size near a 'Geometry.Vertex'.
-- This .geo variable is the 4th parameter to all Points in Gmsh script. Eg: Point(1) = {0, 0, 0, lc};
-- Needs to be output at the start of the .geo file.
writeLC :: B.ByteString
writeLC = "lc = 1e-0;"
writeLC1 :: B.ByteString
writeLC1 = "lc = 1e-1;"
writeLC2 :: B.ByteString
writeLC2 = "lc = 1e-2;"
writeLC3 :: B.ByteString
writeLC3 = "lc = 1e-3;"

-- | Generate gmsh script for a line.
genLineScript :: Env.Id Env.LineInt -> Env.Id Env.PointInt -> Env.Id Env.PointInt -> B.ByteString
genLineScript (Env.LineId (Env.LineInt' lineId)) (Env.PointId (Env.PointInt' pointId1)) (Env.PointId (Env.PointInt' pointId2)) =
  [i|\nLine(#{lineId}) = {#{pointId1},#{pointId2}};|] :: B.ByteString

-- | Generate gmsh script for a curve loop.
genCurveLoopScript :: Env.Id Env.CurveLoopInt -> [Env.Id Env.LineInt] -> B.ByteString
genCurveLoopScript (Env.CurveLoopId (Env.CurveLoopIntP curveLoopId)) lines = 
  [i|\nCurve Loop(#{curveLoopId}) = {#{unwords $ (showIds Env.evalLineId) lines} };|] :: B.ByteString


-- | Generate the gmsh script Bytestring for a Plane Surface
genPlaneSurfaceScript :: Env.Id Env.PlaneSurfaceInt -> [Env.Id Env.CurveLoopInt] -> B.ByteString
genPlaneSurfaceScript  (Env.PlaneSurfaceId (Env.PlaneSurfaceIntP  planeSurfaceId)) curveLoopIds = do
  [i|\nPlane Surface(#{planeSurfaceId}) = {#{unwords $ (showIds Env.evalCurveLoopId) curveLoopIds} };|] :: B.ByteString
    
-- Break a list of Ids, such as LineInt or CurveLoopInt, into strings that can be printed by Interpolate as the arg list to a gmsh fx.    
showIds :: (a -> Int) -> [a] -> [String]
showIds evaluator innerLines =
    showIdsRecur ( map evaluator  innerLines) []
    where
    showIdsRecur [] workingList = reverse workingList
    showIdsRecur [x] workingList = reverse $ show x : workingList
    showIdsRecur (x:xs) workingList = showIdsRecur xs $ (show x ++ ","):workingList
  
  

-- | Write the gmsh line script to handle, which should be .geo file.
lineWriter :: Handle -> Env.Id Env.LineInt -> Env.Id Env.PointInt -> Env.Id Env.PointInt -> IO (Env.Id Env.LineInt)
lineWriter handle' lineId pointId1 pointId2 = do
  B.hPut handle' $ genLineScript lineId pointId1 pointId2
  return lineId

-- | Don't bother writing the Gmsh line script to handle, which will be stdout for tests.
nullLineWriter :: Handle -> Env.Id Env.LineInt -> Env.Id Env.PointInt -> Env.Id Env.PointInt -> IO (Env.Id Env.LineInt)
nullLineWriter _ lineId _ _ = return lineId

--Handles point writing, when nothing is to be written
nullPointWriter :: Handle -> Env.PointIdStatus -> V.Vertex -> IO (Env.Id Env.PointInt)
nullPointWriter _ (Env.PointIdAlreadyExisted pointId) _ = return pointId
nullPointWriter _ (Env.PointIdDidNotExist pointId) _ = return pointId


pointWriter :: Handle -> Env.PointIdStatus -> V.Vertex -> IO (Env.Id Env.PointInt)
pointWriter handle' (Env.PointIdDidNotExist pointId) vertex = do
  B.hPut handle' $ genPointScript vertex pointId
  return pointId
pointWriter _ (Env.PointIdAlreadyExisted pointId) _ = return pointId

  
nullCurveLoopWriter :: Handle -> Env.Id Env.CurveLoopInt -> [Env.Id Env.LineInt] -> IO (Env.Id Env.CurveLoopInt)
nullCurveLoopWriter _ curveLoopId _ = return curveLoopId

curveLoopWriter :: Handle -> Env.Id Env.CurveLoopInt -> [Env.Id Env.LineInt] -> IO (Env.Id Env.CurveLoopInt)
curveLoopWriter handle' curveLoopId lineIds = do
  B.hPut handle' $ genCurveLoopScript curveLoopId  lineIds
  return curveLoopId

nullPlaneSurfaceWriter :: Handle -> Env.Id Env.PlaneSurfaceInt -> [Env.Id Env.CurveLoopInt] -> IO (Env.Id Env.PlaneSurfaceInt)
nullPlaneSurfaceWriter _ planeSurfaceId _ = return planeSurfaceId

planeSurfaceWriter :: Handle -> Env.Id Env.PlaneSurfaceInt -> [Env.Id Env.CurveLoopInt] -> IO (Env.Id Env.PlaneSurfaceInt)
planeSurfaceWriter handle' planeSurfaceId curveLoopIds = do
  B.hPut handle' $ genPlaneSurfaceScript planeSurfaceId  curveLoopIds
  return planeSurfaceId


  
