{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{- |
Supply specialized lists that guaranteed minimum length of 1

import qualified List.Safe1 as L1
-}
module List.Safe1(
                  ToSafeList1(..),
                  SafeList1(..), IntSafe1List,
                  CurveIdSafe1List, PlaneSurfaceSafe1List, evalSafeList1, appendSafeList1) where
import RIO
import qualified RIO.List as L
import qualified Utils.Environment as Env
import qualified Utils.Exceptions as Hex
import Utils.Add
import qualified List.Base as LB

data SafeList1 a b where
     Nil :: SafeList1 a LB.Empty
     Cons:: a -> [a] -> SafeList1 a b -> SafeList1 a LB.NonEmptyID
     
-- Provide show instance for testing.
instance Show (SafeList1 (Env.Id Env.PointInt) LB.NonEmptyID) where
 show ((Cons x zs _)) = "Cons x: " ++ show x ++ " zs: " ++ show zs

 
type CurveIdSafe1List = SafeList1 (Env.Id Env.CurveLoopInt) LB.NonEmptyID
type PlaneSurfaceSafe1List = SafeList1 (Env.Id Env.PlaneSurfaceInt) LB.NonEmptyID
type IntSafe1List = SafeList1 Int LB.NonEmptyID



-- | Extract the 'SafeList3' as a regular list.
evalSafeList1 :: SafeList1 a LB.NonEmptyID -> [a]
evalSafeList1 (Cons x zs _) = x:zs

-- | For creating lists with guaranteed length >= 3, and other qualities depending on the contained type.
class ToSafeList1 a b | b -> a where
  toSafeList1 :: a -> Either Hex.HasMeshException b
  


instance ToSafeList1 [Env.Id Env.CurveLoopInt] CurveIdSafe1List where
  toSafeList1 [] = Left $ Hex.SafeList1MinError "length == 0"
  toSafeList1 [x] = Right $ Cons x [] Nil
  toSafeList1 (x:xs) = Right $ Cons x xs Nil

instance ToSafeList1 [Env.Id Env.PlaneSurfaceInt] PlaneSurfaceSafe1List where
  toSafeList1 [] = Left $ Hex.SafeList1MinError "length == 0"
  toSafeList1 [x] = Right $ Cons x [] Nil
  toSafeList1 (x:xs) = Right $ Cons x xs Nil

instance ToSafeList1 [Int] IntSafe1List where
  toSafeList1 [] = Left $ Hex.SafeList1MinError "length == 0"
  toSafeList1 [x] = Right $ Cons x [] Nil
  toSafeList1 (x:xs) = Right $ Cons x xs Nil

  
instance Add CurveIdSafe1List where
  (Cons x [] Nil) +++ (Cons x' [] Nil) = Cons x [x'] Nil
  (Cons x [] Nil) +++ (Cons x' xs' Nil) = Cons x (x':xs') Nil
  (Cons x xs Nil) +++ (Cons x' [] Nil) = Cons x (xs ++ [x']) Nil
  (Cons x xs Nil) +++ (Cons x' xs' Nil) = Cons x (xs ++ (x':xs')) Nil

instance Add IntSafe1List where
  (Cons x [] Nil) +++ (Cons x' [] Nil) = Cons x [x'] Nil
  (Cons x [] Nil) +++ (Cons x' xs' Nil) = Cons x (x':xs') Nil
  (Cons x xs Nil) +++ (Cons x' [] Nil) = Cons x (xs ++ [x']) Nil
  (Cons x xs Nil) +++ (Cons x' xs' Nil) = Cons x (xs ++ (x':xs')) Nil 


-- | Append a value to a 'SafeList1'
appendSafeList1 :: a -> SafeList1 a LB.NonEmptyID -> SafeList1 a LB.NonEmptyID
appendSafeList1 appendMe (Cons x xs _) = Cons appendMe  (x:xs) Nil  
