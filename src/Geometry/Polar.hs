{-# LANGUAGE NoImplicitPrelude #-}

{- |
Polar Geometry. 

import qualified Geometry.Polar as Polar
-}
module Geometry.Polar(PolarCoordinate(), Radius(..), Degree(..), polarToXaxis, polarToYaxis, polarToZaxis, newPolarCoordinate, newPolarCoordinateFromTuple,
                     toVertex, newVertexFromPolarCoordinatesTuples) where



import RIO
import qualified Geometry.Vertex as V
import qualified Geometry.Axis as Axis

data PolarCoordinate = PolarCood {_angle :: Degree, _radius :: Radius, _pc_zAxis :: Axis.ZAxis}
  deriving (Show,Eq)

newtype Radius = Radius {_rad_length :: Double} deriving (Show,Eq)
newtype Degree = Degree {_degree :: Double} deriving (Show,Eq)
newtype Radian = Radian {_rad :: Double} deriving (Show,Eq)

data Origin = Origin {_xOrigin :: Axis.XAxis, _yOrigin :: Axis.YAxis, _zOrigin :: Axis.ZAxis}

-- | Generate a new 'PolarCoordinate'
newPolarCoordinate :: Degree -> Radius -> Axis.ZAxis -> PolarCoordinate
newPolarCoordinate = PolarCood

-- | A weakly typed constructor to allow easy creation from [(Double,Double,Double)] or from a database.
newPolarCoordinateFromTuple :: (Double, Double, Double) -> PolarCoordinate
newPolarCoordinateFromTuple (degree, radius, zAxis) =
  newPolarCoordinate (Degree degree) (Radius radius) (Axis.ZAxis zAxis)
  

  

degreeToRadians :: Degree -> Radian
degreeToRadians (Degree degree) = Radian $ degree * (pi/180) 
                           
polarToXaxis :: PolarCoordinate -> Axis.XAxis
polarToXaxis (PolarCood degree (Radius radius) _) =
  let
    (Radian radian) = degreeToRadians degree
  in
    Axis.XAxis $ radius * cos radian


polarToYaxis :: PolarCoordinate -> Axis.YAxis
polarToYaxis (PolarCood degree (Radius radius) _) =
  let
    (Radian radian) = degreeToRadians degree
  in
    Axis.YAxis $ radius * sin radian

polarToZaxis :: PolarCoordinate -> Axis.ZAxis
polarToZaxis (PolarCood _ _ zAxis) = zAxis
    

newVertexFromPolarCoordinatesTuples :: [(Double, Double, Double)] -> [V.Vertex]
newVertexFromPolarCoordinatesTuples values =
  map (toVertex . newPolarCoordinateFromTuple) values

toVertex :: PolarCoordinate -> V.Vertex
toVertex polarCood =
  let
    (Axis.XAxis x) = polarToXaxis polarCood
    (Axis.YAxis y) = polarToYaxis polarCood
    (Axis.ZAxis z) = polarToZaxis polarCood
    
    
  in
    V.newVertex x y z    
    
    
  
