{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{- | Supply 'Id's. 

Imported as part of Gmsh.Gmsh: import qualified Gmsh.Gmsh as Gmsh
For internal import: import qualified Gmsh.ID as ID
-}
module Gmsh.ID(incr, Id(..)) where

import Import
import Run
import RIO.Process
import RIO
import qualified Paths_HasMesh
import qualified RIO.Map as Map



import qualified Data.Hashable as H
import qualified Geometry.Geometry as Geo


-- Uses GADTs in order to implement the typecase pattern. See book: Haskell Design Patterns.
-- Using GADTs has not gained me anything, other than getting some experience with GADT's.
-- By having them all be of type Int, they can all use the incr fx.
-- | The gmsh <Point Line etc> ID that is associated with a < 'Vertex' Line etc >
data Id id where
  PointId :: Int -> Id Int
  LineId  :: Int -> Id Int
  

deriving instance Show (Id a)
deriving instance  Eq (Id a)


-- | Increment an ID by 1.
--
--  Used for supplying Gmsh 'Id'
incr :: Id Int -> Id Int
incr (PointId  int) = PointId $ int + 1
incr (LineId   int) = LineId $ int + 1


----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
--The pre-GADT version. Keep around until Lines are implemented in case it GADTs cause too much trouble.

{-
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


-}
