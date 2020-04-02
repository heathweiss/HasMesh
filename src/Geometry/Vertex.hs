{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{- |
Supply a basic 3D geometrical vertex.
Supply a GPoint type that will add an Id to it.
Add additional functionality for creating and tracking Ids and vectors.
-}
module Geometry.Vertex(Vertex(), pattern Vertex',  newVertex) where


import Import
import Run
import RIO.Process
import qualified Paths_HasMesh
import qualified Data.Hashable as H
import qualified Geometry.ID as ID

pattern Vertex' x y z <- Vertex x y z


{- | 
Vertex in 3D geometry.
Has no constructor as the axis values need to be truncated to 2 decimal places for equality and hashing purposes.
-}
data Vertex =  Vertex { _xAxis :: Double, _yAxis :: Double, _zAxis :: Double }
           
              deriving (Show, Typeable, Data, Eq)

{- |
Task:
Create a new Vertex where all 3 axis have been truncated to 2 decimal places.
-}
newVertex :: Double -> Double -> Double -> Vertex
newVertex x y z =
  let
    truncate2 :: Double -> Double
    truncate2  val =
      let
        t = 100 --which is 10^precision. If want to be 1/1000 it would be 10^3. 
      in
        (fromIntegral(floor(val*t)))/t
  in
    Vertex (truncate2 x) (truncate2 y) (truncate2 z)


-- | Creates a key for the vertex map that stores the gmsh PointId. Is based on the hash of x y z values.
instance H.Hashable Vertex where
    -- | Perhaps this should be hidden, and always just use 'hash'
    hashWithSalt s (Vertex x y z) =
        s `H.hashWithSalt`
        x `H.hashWithSalt`
        y `H.hashWithSalt` z
    hash vertex =
      1 `H.hashWithSalt` vertex

