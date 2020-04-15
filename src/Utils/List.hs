{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

{- |
Supply specialized lists.

import qualified Utils.List as L
-}
module Utils.List(SafeList3(..), NonEmptyID(), safeHead3, evalSafeList3, ToSafeList3(..)) where

import RIO
import qualified Gmsh.ID as ID
import qualified Utils.Exceptions as Hex


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
     Cons:: a -> a -> a -> [a] -> SafeList3 a b -> SafeList3 a NonEmptyID
 
-- Provide show instance for testing.
instance Show (SafeList3 (ID.Id ID.PointInt) NonEmptyID) where
 show ((Cons x y z zs _)) = "Cons x: " ++ show x ++ " y: " ++ show y ++ " z: " ++ show z ++ " zs: " ++ show zs  

-- Provide show instance for testing.
instance Show (SafeList3 (ID.Id ID.LineInt) NonEmptyID) where
 show ((Cons x y z zs _)) = "Cons x: " ++ show x ++ " y: " ++ show y ++ show z ++ " zs: " ++ show zs 
 
-- Provide show instance of 'SafeList3' for testing.
instance Eq (SafeList3 (ID.Id ID.PointInt) NonEmptyID) where
  ((Cons x y z zs _)) == ((Cons x' y' z' zs' _)) = (x == x') && (y == y') && (z == z') && (zs == zs') 

-- Provide show instance of 'SafeList3' for testing.
instance Eq (SafeList3 (ID.Id ID.LineInt) NonEmptyID) where
  ((Cons x y z zs _)) == ((Cons x' y' z' zs' _)) = (x == x') && (y == y') && (z == z') && (zs == zs') 

-- | Get the head of a 'SafeList3'
safeHead3 :: SafeList3 a NonEmptyID -> a
safeHead3 (Cons x _ _ _ _) = x
--runSafeHead = safeHead $ Cons "hi" "there" ["how is your", "pie there" ] Nil
--runSafeHead2 = safeHead $ Cons "hi" "there" [] Nil

evalSafeList3 :: SafeList3 a NonEmptyID -> [a]
evalSafeList3 (Cons x y z zs _) = x:y:z:zs

type LineIdSafe3List = SafeList3 (ID.Id ID.LineInt) NonEmptyID

class ToSafeList3 a b | b -> a where
  toSafeList3 :: a -> Either Hex.HasMeshException b

instance ToSafeList3 [ID.Id ID.LineInt] LineIdSafe3List where
  toSafeList3 [] = Left $ Hex.ZeroLengthName "length == 0"
  toSafeList3 [_] = Left $ Hex.ZeroLengthName "length == 1"
  toSafeList3 [_,_] = Left $ Hex.ZeroLengthName "length == 2"
  toSafeList3 [x,y,z] = Right $ Cons x y z [] Nil
  toSafeList3 (x:y:z:zs) = Right $ Cons x y z zs Nil
  
  
  

-------------------------------------------------------------------------------------------------------------------------------
--attempt to create a typecase pattern to convert [a] -> Either Hex.HasMeshException (SafeList3 a b)
--Can't get it to compile.

--type LineIdSafe3List = SafeList3 (ID.Id ID.LineInt) NonEmptyID
--type PointIdList = SafeList3 (ID.Id ID.PointInt) NonEmptyID

--toSafe3List :: [a] ->  Either Hex.HasMeshException (SafeList3 a b)
--toSafe3List [ID.LineId (ID.LineInt int), ID.LineId (ID.LineInt int2), ID.LineId (ID.LineInt int3) ] = _
--toSafe3List (a:b:c:ys) = Right $ Cons (ID.LineId (ID.LineInt a)) (ID.LineId (ID.LineInt b)) (ID.LineId (ID.LineInt c)) ys Nil
                                                                                                                                                 
