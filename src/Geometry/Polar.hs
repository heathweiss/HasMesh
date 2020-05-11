{-# LANGUAGE NoImplicitPrelude #-}

{- |
Use polar math to convert a 'Degree' and 'Radius' to a 'Geometry.Vertex.Vertex' 

import qualified Geometry.Polar as Polar
-}
module Geometry.Polar(newVertexes) where

import RIO
import qualified Geometry.Vertex as V
import qualified Geometry.Axis as Axis

-- | Convert [('Double','Double')] to ['Geometry.Vertex.Vertex'], using polar math, where the [('Double','Double')] represents [(degree,radius)].
--
-- The x y z axis are the origin of the calculation.
newVertexes :: Axis.XAxis -> Axis.YAxis -> Axis.ZAxis -> [(Double,Double)] -> [V.Vertex]
newVertexes (Axis.XAxis x) (Axis.YAxis y) (Axis.ZAxis zAxis) degreeRadiusDoubles =
    newVertexesRecur degreeRadiusDoubles []
    where
      newVertexesRecur :: [(Double,Double)] -> [V.Vertex] ->  [V.Vertex]
      newVertexesRecur    []                   workingList =  reverse workingList
      newVertexesRecur    ((degree,radius):xs) workingList =
        newVertexesRecur  xs                   (newVertex:workingList)
       where
       newVertex ::  V.Vertex
       newVertex =
         V.newVertex
           (x + (radius * cos radian))
           (y + (radius * sin radian))
           zAxis
         where
           radian = degree * (pi/180)
      
