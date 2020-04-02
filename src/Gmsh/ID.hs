{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Supply functionality for Ids such as 'Geometry.PointId' and -}
module Gmsh.ID(Increment(..), PointId(..)) where

import Import
import Run
import RIO.Process
import qualified Paths_HasMesh


-- | Increment an ID by 1.
--
--  Used for supplying ID's for 'Geometry.PointId' etc.
class Increment a where
  incr :: a -> a

-- | The gmsh Point ID that is associated with a 'Vertex'
data PointId = PointId {getPointId :: Int}
  deriving (Eq,Show)

instance Increment PointId where
  incr (PointId id) = PointId $ id + 1
