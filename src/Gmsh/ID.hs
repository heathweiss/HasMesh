{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Supply 'PointId' ADT and associated classes. Is done in a separate module from functionality, to avoid cycle errors.

Imported as part of Gmsh.Gmsh: import qualified Gmsh.Gmsh as Gmsh
For internal import: import qualified Gmsh.ID as ID
-}
module Gmsh.ID(Increment(..), PointId(..)) where

import Import
import Run
import RIO.Process
import RIO
import qualified Paths_HasMesh
import qualified RIO.Map as Map



import qualified Data.Hashable as H
import qualified Geometry.Geometry as Geo


-- | Increment an ID by 1.
--
--  Used for supplying 'PointId' for 'Geometry.Vertex.Vertex' etc.
class Increment a where
  incr :: a -> a -- > Increment the value.

-- | The gmsh Point ID that is associated with a 'Vertex'
data PointId = PointId {_getPointId :: Int}
  deriving (Eq,Show)

instance Increment PointId where
  incr (PointId id) = PointId $ id + 1 


               
