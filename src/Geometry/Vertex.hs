{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{- |
Supply a basic 3D geometrical vertex.
Supply a GPoint type that will add an Id to it.
Add additional functionality for creating and tracking Ids and vectors.

import qualified Geometry.Vertex as V

or import with 'Geometry.Geometry'
-}
module Geometry.Vertex(Vertex(),  pattern Vertex',  newVertex, ) where
import Import
import Run
import RIO.Process
import qualified Paths_HasMesh
import qualified Data.Hashable as H
import qualified Geometry.Axis as Axis

pattern Vertex' x y z <- Vertex x y z


{- | 
Vertex in 3D geometry.
Has no constructor as the axis values need to be truncated to 2 decimal places for equality and hashing purposes.

To create a Gmsh Id for a Vertex, in the form of a 'Gmsh.ID' 'Gmsh.PointInt', use 'Gmsh.Point.toPoints'.
-}
data Vertex =  Vertex { v_xAxis :: Axis.XAxis, v_yAxis :: Axis.YAxis, v_zAxis :: Axis.ZAxis }
  deriving (Show, Typeable, Data, Eq)




{- |
Task:
Create a new Vertex where all 3 axis have been rounded to 2 decimal places.
-}
newVertex :: Double -> Double -> Double -> Vertex
newVertex x y z =
  let
    round_ :: Double -> Double
    round_  val =
      let
        t = 100 --which is 10^precision. If want to be 1/1000 it would be 10^3. 
      in
        --(fromIntegral(floor(val*t)))/t orig stack overflow version to truncate.
        --But truncate fails in Geometry.Polar as rounding is req'd to get accuracy within 1/100th.
        --Doing the same with round, rounds to the nearest 2 places.
        fromIntegral(round(val*t))/t
  in
    Vertex (Axis.XAxis $ round_ x) (Axis.YAxis $ round_ y) (Axis.ZAxis $ round_ z)
    

-- | Creates a key for the vertex map that stores the gmsh PointId. Is based on the hash of the rounded x y z values.
instance H.Hashable Vertex where
    -- | Perhaps this should be hidden, and always just use 'hash'
    hashWithSalt s (Vertex (Axis.XAxis x) (Axis.YAxis y) (Axis.ZAxis z)) =
        -- use show to create unique hashes for Vertex that only differ by sign of 1 or more axis.
        s `H.hashWithSalt`
        show x `H.hashWithSalt`
        show y `H.hashWithSalt` show z
    hash vertex =
      1 `H.hashWithSalt` vertex


