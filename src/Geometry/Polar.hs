{-# LANGUAGE NoImplicitPrelude #-}

{- |
Polar Geometry. 

import qualified Geometry.Polar as Polar
-}
module Geometry.Polar(newVertexes) where



import RIO
import qualified Geometry.Vertex as V
import qualified Geometry.Axis as Axis
import qualified Data.Bifunctor as Bifunctor
--import Lens.Micro



newtype Radius = Radius {r_length :: Double} deriving (Show,Eq)
newtype Degree = Degree {d_degree :: Double} deriving (Show,Eq)
newtype Radian = Radian {r_radian :: Double} deriving (Show,Eq)

-- | Convert [('Double','Double')] to ['Geometry.Vertex.Vertex'], using polar math, where the [('Double','Double')] is used as a [(Degree,Radius)] 
newVertexes :: Axis.XAxis -> Axis.YAxis -> Axis.ZAxis -> [(Double,Double)] -> [V.Vertex]
newVertexes (Axis.XAxis x) (Axis.YAxis y) (Axis.ZAxis zAxis) degreeRadiiAsDoubles =
  toVertexes  
  where
  toVertexes ::  [V.Vertex]
  toVertexes  =
    toVertexesRecur  ( map (Bifunctor.bimap Degree Radius ) degreeRadiiAsDoubles) []
    where
      toVertexesRecur :: [(Degree,Radius)] -> [V.Vertex] -> [V.Vertex]
      toVertexesRecur    []                   workingList =  reverse workingList
      toVertexesRecur    [(degree,radius)]    workingList =  reverse $ toVertex degree radius:workingList
      toVertexesRecur    ((degree,radius):xs) workingList =
       toVertexesRecur   xs                   (toVertex degree radius:workingList)
      
      toVertex :: Degree -> Radius -> V.Vertex
      toVertex (Degree degree) (Radius radius) =
       V.newVertex xAxis yAxis zAxis
       where
         -- Polar math calculations use radians, not degrees.
        (Radian radian) = Radian $ degree * (pi/180)
        (Axis.XAxis xAxis) = Axis.XAxis $ x + (radius * cos radian)
        (Axis.YAxis yAxis) = Axis.YAxis $ y + (radius * sin radian)
        --zAxis doesn't need modification. 
       
        
      
    
      
    
    
    
  


{-
newVertexes :: Origin -> [(Double,Double)] -> [V.Vertex]
newVertexes (Origin (Axis.XAxis x) (Axis.YAxis y) ((Axis.ZAxis zAxis))) degreeRadiiAsDoubles =
  
  toDegreeRadius  
  where
  
  toDegreeRadius ::  [V.Vertex]
  toDegreeRadius  =
    let
      toDegreeRadiusRecur :: [(Degree,Radius)] -> [V.Vertex] -> [V.Vertex]
      toDegreeRadiusRecur  [] workingList =  reverse workingList
      toDegreeRadiusRecur  [(degree,radius)] workingList =  reverse $ toVertex  degree radius:workingList
      toDegreeRadiusRecur  ((degree,radius):xs) workingList =
        toDegreeRadiusRecur  xs  (toVertex  degree radius :workingList)
        
    in
      toDegreeRadiusRecur  ( map (Bifunctor.bimap Degree Radius ) degreeRadiiAsDoubles) []

  toVertex :: Degree -> Radius -> V.Vertex
  toVertex (Degree degree) (Radius radius) =
   let
     -- Polar math calculations require radians, not degrees.
    (Radian radian) = Radian $ degree * (pi/180)
    (Axis.XAxis xAxis) = Axis.XAxis $ x + (radius * cos radian)
    (Axis.YAxis yAxis) = Axis.YAxis $ y + (radius * sin radian)
     
   in
    V.newVertex xAxis yAxis zAxis
    
  



newVertexes :: Origin -> [(Double,Double)] -> [V.Vertex]
newVertexes (Origin (Axis.XAxis x) (Axis.YAxis y) ((Axis.ZAxis zAxis))) degreeRadiiAsDoubles =
  
  toDegreeRadius  
  where
  
  toDegreeRadius ::  [V.Vertex]
  toDegreeRadius  =
    let
      toDegreeRadiusRecur :: [(Degree,Radius)] -> [V.Vertex] -> [V.Vertex]
      toDegreeRadiusRecur  [] workingList =  reverse workingList
      toDegreeRadiusRecur  [(degree,radius)] workingList =  reverse $ toVertex (DegreeRadius degree radius):workingList
      toDegreeRadiusRecur  ((degree,radius):xs) workingList =
        toDegreeRadiusRecur  xs  (toVertex( DegreeRadius degree radius) :workingList)
        
    in
      toDegreeRadiusRecur  ( map (Bifunctor.bimap Degree Radius ) degreeRadiiAsDoubles) []

  toVertex :: DegreeRadius -> V.Vertex
  toVertex (DegreeRadius (Degree degree) (Radius radius) ) =
   let
     -- Polar math calculations require radians, not degrees.
    (Radian radian) = Radian $ degree * (pi/180)
    (Axis.XAxis xAxis) = Axis.XAxis $ x + (radius * cos radian)
    (Axis.YAxis yAxis) = Axis.YAxis $ y + (radius * sin radian)
     
   in
    V.newVertex xAxis yAxis zAxis







-- | Convert [('Double','Double')] to ['Geometry.Vertex.Vertex'], centered on an 'Origin'    
newVertexes :: Origin -> [(Double,Double)] -> [V.Vertex]
newVertexes (Origin (Axis.XAxis x) (Axis.YAxis y) ((Axis.ZAxis zAxis))) degreeRadiiAsDoubles =
  
  map toVertex toDegreeRadius  
  where
  
  toDegreeRadius ::  [DegreeRadius]
  toDegreeRadius  =
    let
      toDegreeRadiusRecur :: [(Degree,Radius)] -> [DegreeRadius] -> [DegreeRadius]
      toDegreeRadiusRecur  [] workingList =  reverse workingList
      toDegreeRadiusRecur  [(degree,radius)] workingList =  reverse $ DegreeRadius degree radius :workingList
      toDegreeRadiusRecur  ((degree,radius):xs) workingList =
        toDegreeRadiusRecur  xs  (DegreeRadius degree radius :workingList)
        
    in
      toDegreeRadiusRecur  ( map (Bifunctor.bimap Degree Radius ) degreeRadiiAsDoubles) []

  toVertex :: DegreeRadius -> V.Vertex
  toVertex (DegreeRadius (Degree degree) (Radius radius) ) =
   let
     -- Polar math calculations require radians, not degrees.
    (Radian radian) = Radian $ degree * (pi/180)
    (Axis.XAxis xAxis) = Axis.XAxis $ x + (radius * cos radian)
    (Axis.YAxis yAxis) = Axis.YAxis $ y + (radius * sin radian)
     
   in
    V.newVertex xAxis yAxis zAxis
    


























-- | Convert [('Degree','Radius')] to ['Geometry.Vertex.Vertex'], centered on an 'Origin'    
newVertexes :: Origin -> [(Double,Double)] -> [V.Vertex]
newVertexes (Origin (Axis.XAxis x) (Axis.YAxis y) ((Axis.ZAxis zAxis))) degreeRadiiAsDoubles =
  
  map toVertex toDegreeRadius  
  where
  
  toDegreeRadius ::  [DegreeRadius]
  toDegreeRadius  =
    let
      toDegreeRadiusRecur :: [(Degree,Radius)] -> [DegreeRadius] -> [DegreeRadius]
      toDegreeRadiusRecur  [] workingList =  reverse workingList
      toDegreeRadiusRecur  [(degree,radius)] workingList =  reverse $ DegreeRadius degree radius :workingList
      toDegreeRadiusRecur  ((degree,radius):xs) workingList =
        toDegreeRadiusRecur  xs  (DegreeRadius degree radius :workingList)
        
    in
      toDegreeRadiusRecur  ( map (Bifunctor.bimap Degree Radius ) degreeRadiiAsDoubles) []
      

  -- Polar math calculations require radians, not degrees.
  degreeToRadians :: Degree -> Radian
  degreeToRadians (Degree degree) = Radian $ degree * (pi/180)
  
  toVertex :: DegreeRadius -> V.Vertex
  toVertex (DegreeRadius degree (Radius radius) ) =
   let
    (Radian radian) = degreeToRadians degree
    (Axis.XAxis xAxis) = Axis.XAxis $ x + (radius * cos radian)
    (Axis.YAxis yAxis) = Axis.YAxis $ y + (radius * sin radian)
     
   in
    V.newVertex xAxis yAxis zAxis
    






















module Geometry.Polar( Radius(..), Degree(..), Origin(..), newVertexes, toDegreeRadius ) where



import RIO
import qualified Geometry.Vertex as V
import qualified Geometry.Axis as Axis
import qualified Data.Bifunctor as Bif
--import Lens.Micro


data PolarCoordinate = PolarCoordinate {_angle :: Degree, _radius :: Radius}
  deriving (Show,Eq)

newtype Radius = Radius {_rad_length :: Double} deriving (Show,Eq)
newtype Degree = Degree {_degree :: Double} deriving (Show,Eq)
newtype Radian = Radian {_rad :: Double} deriving (Show,Eq)

-- | Used as center of polar cood.
data Origin = Origin {_xOrigin :: Axis.XAxis, _yOrigin :: Axis.YAxis, _zOrigin :: Axis.ZAxis} deriving (Show, Eq)

  


                                  
-- | Convert [('Degree','Radius')] to ['Geometry.Vertex.Vertex'], centered on an 'Origin'    
newVertexes :: Origin -> [(Degree,Radius)] -> [V.Vertex]
newVertexes (Origin (Axis.XAxis x) (Axis.YAxis y) ((Axis.ZAxis zAxis))) degreeRadius =
  
  map toVertexFromPolarCood $ toPolarCoodsFromDegreeRadius degreeRadius
  where
  toPolarCoodsFromDegreeRadius ::  [(Degree,Radius)] -> [PolarCoordinate]
  toPolarCoodsFromDegreeRadius  polarDegreeRadii =
    let
      toPolarCoodsRecur :: [(Degree,Radius)] -> [PolarCoordinate] -> [PolarCoordinate]
      toPolarCoodsRecur  [] workingList =  reverse workingList
      toPolarCoodsRecur  [(degree,radius)] workingList =  reverse $ PolarCoordinate degree radius :workingList
      toPolarCoodsRecur  ((degree,radius):xs) workingList =
        toPolarCoodsRecur  xs  (PolarCoordinate degree radius :workingList) 
    in
      toPolarCoodsRecur  polarDegreeRadii []

  -- Polar math calculations require radians, not degrees.
  degreeToRadians :: Degree -> Radian
  degreeToRadians (Degree degree) = Radian $ degree * (pi/180)
  
  toVertexFromPolarCood :: PolarCoordinate -> V.Vertex
  toVertexFromPolarCood (PolarCoordinate degree (Radius radius) ) =
   let
    (Radian radian) = degreeToRadians degree
    (Axis.XAxis xAxis) = Axis.XAxis $ x + (radius * cos radian)
    (Axis.YAxis yAxis) = Axis.YAxis $ y + (radius * sin radian)
     
   in
    V.newVertex xAxis yAxis zAxis
    
  

-- | Converts a [('Double','Double')] into a [('Degree','Radius')], which is required to create ['Geomentry.Vertex.Vertex'] using polar math.
-- Is a convenience which makes is easier, though less safe, to make the [('Degree','Radius')].
toDegreeRadius :: [(Double,Double)] ->  [(Degree,Radius)]
toDegreeRadius  =
  map (Bif.bimap Degree Radius )

-}
