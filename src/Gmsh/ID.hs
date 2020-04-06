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
module Gmsh.ID(Id(..), PointInt(..), newPointId, incr) where

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
newtype PointInt = PointInt Int deriving (Show,Eq)
newtype LineInt = LineInt Int deriving (Show,Eq)

data Id id where
  PointId :: PointInt -> Id PointInt
  LineId  :: LineInt -> Id LineInt
  

deriving instance Show (Id a)
deriving instance  Eq (Id a)


-- | Increment an ID by 1.
--
--  Used for supplying Gmsh 'Id'
incr :: Id a -> Id a
incr (PointId (PointInt int)) = PointId $ PointInt $ int + 1
incr (LineId  (LineInt int)) = LineId $ LineInt $ int + 1

newPointId :: Int -> Id PointInt
newPointId int = PointId $ PointInt int


incr1 :: Id PointInt -> Id PointInt
incr1 (PointId  (PointInt int)) = PointId $ PointInt $ int + 1
--incr (LineId  (LineInt int)) = LineId $ LineInt $ int + 1
--Does not compile as it does not return a Id Int.
--This is good, as it limits it to PointId

incr2 :: Id LineInt -> Id LineInt
incr2 (LineId  (LineInt int)) = LineId $ LineInt $ int + 1


----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
--1 GADT version. Keep around until version 2 is done

{-
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
-}

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
