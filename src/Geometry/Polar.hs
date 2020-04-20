{-# LANGUAGE NoImplicitPrelude #-}

module Geometry.Polar() where

import RIO


data PolarCoordinate = PolarCd {_theta :: Degree, _radius :: Radius, _pc_zAxis :: ZAxis}
data RectangularCoordinate = RecCood {rc_xAxis :: XAxis, rc_yAxis :: YAxis, rc_zAsis :: ZAxis}


newtype XAxis = XAxis {_xAxis :: Double}
newtype YAxis = YAxis {_yAxis :: Double}
newtype ZAxis = ZAxis {_zAxis :: Double}
newtype Radius = Radius {_rad_length :: Double}
newtype Degree = Degree {_degree :: Double}
newtype Radian = Radian {_rad :: Double}

data Origin = Origin {_xOrigin :: XAxis, _yOrigin :: YAxis, _zOrigin :: ZAxis}


degreeToRadians :: Degree -> Radian
degreeToRadians (Degree degree) = Radian $ degree * 0.0174533

                           
polarToXaxis :: PolarCoordinate -> XAxis
polarToXaxis (PolarCd degree (Radius radius) _) =
  let
    (Radian radian) = degreeToRadians degree
  in
    XAxis $ radius * cos radian


polarToYaxis :: PolarCoordinate -> YAxis
polarToYaxis (PolarCd degree (Radius radius) _) =
  let
    (Radian radian) = degreeToRadians degree
  in
    YAxis $ radius * sin radian

polarToZaxis :: PolarCoordinate -> ZAxis
polarToZaxis (PolarCd _ _ zAxis) = zAxis
    
polarToRectangular :: PolarCoordinate -> RectangularCoordinate
polarToRectangular polarCood =
  RecCood (polarToXaxis polarCood) (polarToYaxis polarCood) (polarToZaxis polarCood)
  
    
    



