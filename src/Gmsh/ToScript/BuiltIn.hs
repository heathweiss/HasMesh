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
module Gmsh.ToScript.BuiltIn(writePoint, writeLine, writeLC1, writeLC2, writeLC3) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Data.String.Interpolate ( i )

import qualified Geometry.Geometry as Geo
--import qualified Gmsh.ID as ID
import qualified Utils.Environment as Env

-- | Output a gmsh point.
writePoint :: Geo.Vertex -> Env.Id Env.PointInt -> B.ByteString
writePoint (Geo.Vertex' x y z) (Env.PointId (Env.PointInt' id)) =
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
writeLC1 :: B.ByteString
writeLC1 = "lc = 1e-1;"
writeLC2 :: B.ByteString
writeLC2 = "lc = 1e-2;"
writeLC3 :: B.ByteString
writeLC3 = "lc = 1e-3;"


writeLine :: Env.Id Env.LineInt -> Env.Id Env.PointInt -> Env.Id Env.PointInt -> B.ByteString
writeLine (Env.LineId (Env.LineInt' lineId)) (Env.PointId (Env.PointInt' pointId1)) (Env.PointId (Env.PointInt' pointId2)) =
  
  [i|\nLine(#{lineId}) = {#{pointId1},#{pointId2}};|] :: B.ByteString

{-
module Gmsh.ToScript.BuiltIn(writePoint, writeLine, writeLC1, writeLC2, writeLC3) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Data.String.Interpolate ( i )

import qualified Geometry.Geometry as Geo
import qualified Gmsh.ID as ID

-- | Output a gmsh point.
writePoint :: Geo.Vertex -> ID.Id ID.PointInt -> B.ByteString
writePoint (Geo.Vertex' x y z) (ID.PointId (ID.PointInt' id)) =
  [i|\nPoint(#{id}) = {#{x},#{y},#{z},lc};|] :: B.ByteString
{-
writePoint :: Geo.Vertex -> ID.Id Int -> B.ByteString
writePoint (Geo.Vertex' x y z) (ID.PointId id) =
  [i|\nPoint(#{id}) = {#{x},#{y},#{z},lc};|] :: B.ByteString

before GADT
writePoint :: Geo.Vertex -> Gmsh.PointId -> B.ByteString
writePoint (Geo.Vertex' x y z) (Gmsh.PointId id) =
  [i|Point(#{id}) = {#{x},#{y},#{z},lc};|] :: B.ByteString
-}

-- | Write the lc var that Gmsh tutorials use to set the mesh size near a 'Geometry.Vertex'.
-- This .geo variable is the 4th parameter to all Points in Gmsh script. Eg: Point(1) = {0, 0, 0, lc};
-- Needs to be output at the start of the .geo file.
writeLC1 :: B.ByteString
writeLC1 = "lc = 1e-1;"
writeLC2 :: B.ByteString
writeLC2 = "lc = 1e-2;"
writeLC3 :: B.ByteString
writeLC3 = "lc = 1e-3;"


writeLine :: ID.Id ID.LineInt -> ID.Id ID.PointInt -> ID.Id ID.PointInt -> B.ByteString
writeLine (ID.LineId (ID.LineInt lineId)) (ID.PointId (ID.PointInt pointId1)) (ID.PointId (ID.PointInt pointId2)) =
  [i|\nLine(#{lineId}) = {#{pointId1},#{pointId2}};|] :: B.ByteString

-}
