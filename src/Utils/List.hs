{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Supply specialized lists.

import qualified Utils.List as L
-}
module Utils.List(SafeList3(..), NonEmptyID(), safeHead3) where

import RIO
import qualified Gmsh.ID as ID

data Empty
data NonEmptyID
deriving instance Show (Empty)
deriving instance Show (NonEmptyID)

-- | Supply a list that has a minumum of 3 elements.
--
-- Known Uses:
--
-- A ['Gmsh.ID' 'Gmsh.LineId'], which is the type used for building Gmsh lines from a ['Geometry.Vertex.Vertex'].
-- There must be at least 3 lines in order to create a closed polygon. This requires a ['Geometry.Vertex.Vertex'] of at least 3 items.
--
-- Eg: 3 'Geometry.Vertex.Vertex' will create a triangle, while 4 would create a square.
data SafeList3 a b where
     Nil :: SafeList3 a Empty
     Cons:: a -> a -> [a] -> SafeList3 a b -> SafeList3 a NonEmptyID
 
--instance Show (SafeList a b) where
--  show Nil = "nil"
--  show ((Cons x y ys _)) = "Cons "
  
--deriving instance Show (SafeList a b)
--deriving instance  Eq (SafeList a)

-- Provide show instance of 'SafeList3' for testing.
instance Show (SafeList3 (ID.Id ID.PointInt) NonEmptyID) where
 show ((Cons x y ys _)) = "Cons x: " ++ show x ++ " y: " ++ show y ++ " ys: " ++ show ys -- ^ Required for testing.
 
-- Provide show instance of 'SafeList3' for testing.
instance Eq (SafeList3 (ID.Id ID.PointInt) NonEmptyID) where
  ((Cons x y ys _)) == ((Cons x' y' ys' _)) = (x == x') && (y == y') && (ys == ys') -- ^ Required for testing.

-- | Get the head of a 'SafeList3'
safeHead3 :: SafeList3 a NonEmptyID -> a
safeHead3 (Cons x y ys _) = x
--runSafeHead = safeHead $ Cons "hi" "there" ["how is your", "pie there" ] Nil
--runSafeHead2 = safeHead $ Cons "hi" "there" [] Nil


