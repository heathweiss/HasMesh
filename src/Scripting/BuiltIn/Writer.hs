{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{- |
Generate Gmsh scripts, such as writing Points, Lines, Curves and comments.

import qualified Scripting.Scripting as Script
-}
module Scripting.BuiltIn.Writer(writeSeparator, writePoint, writeComment, writeLC1, writeLC2, writeLC3) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Data.String.Interpolate ( i )

import qualified Geometry.Geometry as Geo
import qualified Gmsh.Gmsh as Gmsh


-- | Output a separator to help format the .geo file into sections.
writeSeparator :: B.ByteString
writeSeparator = "///////////////////////////////////////////////////////////////"

-- | Output a gmsh comment.
writeComment :: T.Text -> B.ByteString
writeComment text =
  [i|\n//#{text}|] :: B.ByteString

-- | Output a gmsh point.
writePoint :: Geo.Vertex -> Gmsh.Id Int -> B.ByteString
writePoint (Geo.Vertex' x y z) (Gmsh.PointId id) =
  [i|\nPoint(#{id}) = {#{x},#{y},#{z},lc};|] :: B.ByteString
{-before GADT
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

