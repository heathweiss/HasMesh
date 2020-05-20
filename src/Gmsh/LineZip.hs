{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{- |
import qualified Gmsh.LineZip as LZ
-}
module Gmsh.LineZip(zipPoints, zipLines, zipCurves, zipPlanes) where
--module Gmsh.Planes.Rectangles which is only take1_1_1 or take 1 1 
--module Gmsh.Planes.Flex
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO

import qualified Geometry.Vertex as V
import qualified Utils.Environment as Env
import qualified Geometry.Geometry as Geo
--import qualified Gmsh.Point as Pts  
import qualified Utils.EnvironmentLoader as EnvLdr 
import qualified Utils.Environment as Env
import qualified Gmsh.Line as Line
--import qualified Gmsh.Point as Pnt
import qualified Gmsh.PlaneSurface as PS
import qualified List.Safe1 as L1
import qualified List.Safe3 as L3
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import qualified Geometry.Axis as Axis
import qualified Geometry.Polar as Polar
import qualified Gmsh.Line as Line
import qualified Gmsh.CurveLoop as CL 
{- |
Add together 2 [Points] like adding lines in ChampCad, to get a [faces], from which nice meshes can be built by Gmsh, in the same way I build meshes.
I can use shapes other than rectangular faces, as gmsh is doing the meshes. 
Should I be working in Vertex, or VertexSafe3List?
This is the first step towards using zipPlanes.
-}
zipPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) =>  L3.PointIdSafe3List -> L3.PointIdSafe3List -> RIO env [L3.PointIdSafe3List]
zipPoints (L3.Cons i1 i2 i3 [] _) (L3.Cons o1 o2 o3 [] _) =
  return
  [(L3.Cons i1 o1 o2  [i2] L3.Nil),
  (L3.Cons i2 o2 o3 [i3] L3.Nil),
  (L3.Cons i3 o3 o1  [i1] L3.Nil)
  ]

zipPoints (L3.Cons i1 i2 i3 [i4] _) (L3.Cons o1 o2 o3 [o4] _) =
  return
  [(L3.Cons i1 o1 o2  [i2] L3.Nil),
  (L3.Cons i2 o2 o3 [i3] L3.Nil),
  (L3.Cons i3 o3 o4  [i4] L3.Nil),
  (L3.Cons i4 o4 o1  [i1] L3.Nil)
  ]

zipPoints (L3.Cons i1 i2 i3 [i4,i5] _) (L3.Cons o1 o2 o3 [o4,o5] _) =
  return
  [(L3.Cons i1 o1 o2  [i2] L3.Nil),
  (L3.Cons i2 o2 o3 [i3] L3.Nil),
  (L3.Cons i3 o3 o4  [i4] L3.Nil),
  (L3.Cons i4 o4 o5  [i5] L3.Nil),
  (L3.Cons i5 o5 o1  [i1] L3.Nil)
  ]
                                                     
zipPoints (L3.Cons i1 i2 i3 (i4:i5:i6:is) _) (L3.Cons o1 o2 o3 (o4:o5:o6:os) _) =
  zipPointsRecur (L3.Cons i4 i5 i6 is L3.Nil) (L3.Cons o4 o5 o6 os L3.Nil)  [L3.Cons i3 o3 o4  [i4] L3.Nil,L3.Cons i2 o2 o3 [i3] L3.Nil, L3.Cons i1 o1 o2  [i2] L3.Nil]
  where
    zipPointsRecur :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => L3.PointIdSafe3List -> L3.PointIdSafe3List -> [L3.PointIdSafe3List] -> RIO env [L3.PointIdSafe3List]
    zipPointsRecur  (L3.Cons ir1 ir2 ir3 [] _) (L3.Cons or1 or2 or3 [] _) workingList =
       return $ reverse $  [L3.Cons ir3 or3 o1  [i1] L3.Nil, L3.Cons ir2 or2 or3 [ir3] L3.Nil, L3.Cons ir1 or1 or2  [ir2] L3.Nil]  ++ workingList
    zipPointsRecur  (L3.Cons ir1 ir2 ir3 (ir4:irs) _) (L3.Cons or1 or2 or3 (or4:ors) _) workingList =
       zipPointsRecur (L3.Cons ir2 ir3 ir4 irs L3.Nil) (L3.Cons or2 or3 or4 ors L3.Nil) $
                      ((L3.Cons ir1 or1 or2 [ir2] L3.Nil)::L3.PointIdSafe3List):workingList
       

-- | Use the results of 'zipPoints' to create lines.
zipLines :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => [L3.PointIdSafe3List] -> RIO env [L3.LineIdSafe3List]
zipLines points = do
  env <- ask
  runRIO env $ zipLinesRecur points []
  where
    zipLinesRecur :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => [L3.PointIdSafe3List] -> [L3.LineIdSafe3List] -> RIO env [L3.LineIdSafe3List]
    zipLinesRecur [] workingList = return $ reverse workingList
    zipLinesRecur (x:xs) workingList = do
      env <- ask
      lines <- runRIO env $ Line.toLines x
      zipLinesRecur xs (lines:workingList)
      
-- | Use the results of 'zipLines' to create curve loops
zipCurves :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => [L3.LineIdSafe3List] -> RIO env [L1.CurveIdSafe1List]
zipCurves lines = do
  env <- ask
  runRIO env $ zipCurvesRecur lines []
  where
    zipCurvesRecur :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => [L3.LineIdSafe3List] -> [L1.CurveIdSafe1List] -> RIO env [L1.CurveIdSafe1List]
    zipCurvesRecur [] workingList = return $ reverse workingList
    zipCurvesRecur (x:xs) workingList = do
      env <- ask
      curves <- runRIO env $ CL.toCurveLoop x
      
      zipCurvesRecur xs (curves:workingList)
      
-- | Use the results of zipCurves to create plane surfaces
zipPlanes :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => [L1.CurveIdSafe1List] -> RIO env [L1.PlaneSurfaceSafe1List]
zipPlanes curves = do
  env <- ask
  runRIO env $ zipPlanesRecur curves []
  
  where
    zipPlanesRecur :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => [L1.CurveIdSafe1List] -> [L1.PlaneSurfaceSafe1List] -> RIO env [L1.PlaneSurfaceSafe1List]
    zipPlanesRecur [] workingList = return $ reverse workingList
    zipPlanesRecur (x:xs) workingList = do
      env <- ask
      planeSurface <- runRIO env $ PS.toPlaneSurface x
      zipPlanesRecur xs (planeSurface:workingList)
      
