{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{- | Supply 'Id's. 

Imported as part of Gmsh.Gmsh: import qualified Gmsh.Gmsh as Gmsh
For internal import: import qualified Gmsh.ID as ID
-}
module Gmsh.ID(Id(..), PointInt(..), LineInt(..), newPointId, initializeIdLineInt, incr, evalLineId) where

import RIO
import qualified RIO.Map as Map


import qualified Data.Hashable as H
import qualified Geometry.Geometry as Geo



-- Uses GADTs in order to implement the typecase pattern. See book: Haskell Design Patterns.
-- Using GADTs has not gained me anything, other than getting some experience with GADT's.
-- By having them all be of type Int, they can all use the incr fx.
-- | The gmsh <Point Line etc> ID that is associated with a < 'Vertex' Line etc >
-- These are used by the 'ID' ADT to give a unique return type for each constructor.
newtype PointInt = PointInt Int deriving (Show,Eq)
newtype LineInt = LineInt Int deriving (Show,Eq)

data Id id where
  PointId :: PointInt -> Id PointInt
  LineId  :: LineInt -> Id LineInt
  

deriving instance Show (Id a)
deriving instance  Eq (Id a)


-- | Increment an 'ID' < 'PointId' 'LineId' > by 1.
--
--  Used for supplying Gmsh ids for 'Geo.Vector' and and line ids for ['Geo.Vector']
incr :: Id a -> Id a
incr (PointId (PointInt int)) = PointId $ PointInt $ int + 1
incr (LineId  (LineInt int)) = LineId $ LineInt $ int + 1

-- | Create a new 'Id PointInt' for the seed value of the 'Environment.Environment' points 'ID' supply.
newPointId :: Int -> Id PointInt
newPointId int = PointId $ PointInt int

-- | Create a new 'Id LineInt' for the seed value of the 'Environment.Environment' line 'ID' supply.
initializeIdLineInt :: Int -> Id LineInt
initializeIdLineInt int = LineId $ LineInt int

evalLineId :: Id LineInt-> LineInt
evalLineId (LineId lineInt) = lineInt

