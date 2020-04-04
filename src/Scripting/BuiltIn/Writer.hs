{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{- |
Generate Gmsh scripts, such as writing Points, Lines, Curves and comments.

import qualified Scripting.Scripting as Script
-}
module Scripting.BuiltIn.Writer(separator, point, comment, lc, newLC1, newLC2, newLC3) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Data.String.Interpolate ( i )

import qualified Geometry.Geometry as Geo
import qualified Gmsh.Gmsh as Gmsh


-- | Output a separator to help format the .geo file into sections.
separator :: B.ByteString
separator = "///////////////////////////////////////////////////////////////"

-- | Output a gmsh comment.
comment :: T.Text -> B.ByteString
comment text =
  [i|\n//#{text}|] :: B.ByteString

-- | Output a gmsh point.
point :: Geo.Vertex -> Gmsh.PointId -> B.ByteString
point (Geo.Vertex' x y z) (Gmsh.PointId id) =
  [i|Point(#{id}) = {#{x},#{y},#{z},lc};|] :: B.ByteString

-- | The mesh size as used by Gmsh when a Point is written.
data LC = LC1 {lc1 :: B.ByteString}
        | LC2 {lc2 :: B.ByteString}
        | LC3 {lc3 :: B.ByteString}

-- | 1e-1 
newLC1 =  LC1 "1e-1"
-- | 1e-2
newLC2 =  LC2 "1e-2"
-- | 1e-3
newLC3 =  LC2 "1e-3"


lc :: LC -> B.ByteString
lc (LC1 lc_) = [i|lc = #{lc_};|] :: B.ByteString
lc (LC2 lc_) = [i|lc = #{lc_};|] :: B.ByteString
lc (LC3 lc_) = [i|lc = #{lc_};|] :: B.ByteString               
