{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Supply a basic 3D geometrical vertex.
Supply a GPoint type that will add an Id to it.
Add additional functionality for creating and tracking Ids and vectors.
-}
module Geometry.Vector(Vertex(..)) where


import Import
import Run
import RIO.Process
import qualified Paths_HasMesh


{- | ------------------------- Vertex------------------------------
Vertexs in 3D geometry.
-}
data Vertex =  Vertex { _xAxis :: Double, _yAxis :: Double, _zAxis :: Double }
           
              deriving (Show, Typeable, Data)


{- | ---------------       instance of equal ---------------
In order to avoid double rounding errors and  trig errors which cause
the same point, and thus CornerPoints, to be != due to tiny differences,
give it a range of < .01, and still allow the points to be equal.
All the type restrictions are to get it to compile.
-}
axisEqual :: (Eq a, Num a, Ord a, Fractional a) => a -> a -> Bool
axisEqual  a b
  
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False


instance Eq Vertex where
    Vertex x y z == Vertex xa ya za
      |  (axisEqual x xa) && (axisEqual y ya)  && (axisEqual z za) = True 
      | otherwise = False
    
