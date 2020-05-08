{-# LANGUAGE NoImplicitPrelude #-}

{- |
Use polar math to convert a 'Degree' and 'Radius' to a 'Geometry.Vertex.Vertex' 

import qualified Geometry.Polar as Polar
-}
module Geometry.Polar(newVertexes) where

import RIO
import qualified Geometry.Vertex as V
import qualified Geometry.Axis as Axis
import qualified Data.Bifunctor as Bifunctor

newtype Radius = Radius {r_length :: Double} deriving (Show,Eq)
newtype Degree = Degree {d_degree :: Double} deriving (Show,Eq)
newtype Radian = Radian {r_radian :: Double} deriving (Show,Eq)

-- | Convert [('Double','Double')] to ['Geometry.Vertex.Vertex'], using polar math, where the [('Double','Double')] are a weakly typed [(Degree,Radius)].
--
-- It uses the <x y z> axis as the center or origin of the calculation.
newVertexes :: Axis.XAxis -> Axis.YAxis -> Axis.ZAxis -> [(Double,Double)] -> [V.Vertex]
newVertexes (Axis.XAxis x) (Axis.YAxis y) (Axis.ZAxis zAxis) degreeRadiusDoubles =
    toVertexes  ( map (Bifunctor.bimap Degree Radius ) degreeRadiusDoubles) []
    where
      toVertexes :: [(Degree,Radius)] ->               [V.Vertex] ->  [V.Vertex]
      toVertexes    []                                 workingList =  reverse workingList
      toVertexes    ((Degree degree,Radius radius):xs) workingList =
        toVertexes  xs                                (newVertex:workingList)
       where
       newVertex ::  V.Vertex
       newVertex =
         V.newVertex xAxis yAxis zAxis
         where
           -- Convert the degrees to radians, as polar math calculations require radians.
          (Radian radian) = Radian $ degree * (pi/180)
          
          (Axis.XAxis xAxis) = Axis.XAxis $ x + (radius * cos radian)
          (Axis.YAxis yAxis) = Axis.YAxis $ y + (radius * sin radian)
          --zAxis never gets modified. 
       
        
      
