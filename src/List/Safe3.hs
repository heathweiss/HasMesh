{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

{- |
Supply specialized lists that guaranteed minimum length of 3

import qualified List.Safe3 as L3
-}
module List.Safe3(SafeList3(..), PointIdSafe3List(), LineIdSafe3List(), VertexSafe3List(), isUnique,
                 safeHead3, evalSafeList3, safeLast3, ToSafeList3(..), reverseSafeList3, appendSafeList3,) where
import RIO
import qualified RIO.List as L
import qualified Utils.Environment as Env
import qualified Utils.Exceptions as Hex
import qualified Geometry.Vertex as V
import qualified List.Base as LB
import List.Base

-- | Supply a list that has a minumum of 3 elements.
--
-- Known Uses:
--
-- A ['Gmsh.ID' 'Gmsh.LineId'], which is the type used for building Gmsh lines from a ['Geometry.Vertex.Vertex'].
-- There must be at least 3 lines in order to create a closed polygon. This requires a ['Geometry.Vertex.Vertex'] of at least 3 items.
--
-- Eg: 3 'Geometry.Vertex.Vertex' will create a triangle, while 4 would create a square.
data SafeList3 a b where
     Nil :: SafeList3 a LB.Empty
     Cons:: a -> a -> a -> [a] -> SafeList3 a b -> SafeList3 a LB.NonEmptyID
 
-- Provide show instance for testing.
instance Show (SafeList3 (Env.Id Env.PointInt) LB.NonEmptyID) where
 show ((Cons x y z zs _)) = "Cons x: " ++ show x ++ " y: " ++ show y ++ " z: " ++ show z ++ " zs: " ++ show zs  

-- Provide show instance for testing.
instance Show (SafeList3 (Env.Id Env.LineInt) LB.NonEmptyID) where
 show ((Cons x y z zs _)) = "Cons x: " ++ show x ++ " y: " ++ show y ++ show z ++ " zs: " ++ show zs

-- Provide show instance for testing.
instance Show (SafeList3 V.Vertex LB.NonEmptyID) where
 show ((Cons x y z zs _)) = "Cons x: " ++ show x ++ " y: " ++ show y ++ show z ++ " zs: " ++ show zs 
 
-- Provide show instance of 'SafeList3' for testing.
instance Eq (SafeList3 (Env.Id Env.PointInt) LB.NonEmptyID) where
  ((Cons x y z zs _)) == ((Cons x' y' z' zs' _)) = (x == x') && (y == y') && (z == z') && (zs == zs') 

-- Provide show instance of 'SafeList3' ('Env.Id' 'Env.LineInt') for testing.
instance Eq (SafeList3 (Env.Id Env.LineInt) LB.NonEmptyID) where
  ((Cons x y z zs _)) == ((Cons x' y' z' zs' _)) = (x == x') && (y == y') && (z == z') && (zs == zs') 

-- Provide show instance of 'SafeList3' ('Env.Id' 'Env.LineInt') for testing.
instance Eq (SafeList3 (V.Vertex) LB.NonEmptyID) where
  ((Cons x y z zs _)) == ((Cons x' y' z' zs' _)) = (x == x') && (y == y') && (z == z') && (zs == zs')




-- | Get the head of a 'SafeList3'
safeHead3 :: SafeList3 a LB.NonEmptyID -> a
safeHead3 (Cons x _ _ _ _) = x

-- | Get the last item in a safelist
safeLast3 :: SafeList3 a LB.NonEmptyID -> a
safeLast3 (Cons x y z (z':zs) _) =
  safeLast3 $ Cons y z z' zs Nil
safeLast3 (Cons x y z [] _) = z

  

-- | Extract the 'SafeList3' as a regular list.
evalSafeList3 :: SafeList3 a LB.NonEmptyID -> [a]
evalSafeList3 (Cons x y z zs _) = x:y:z:zs

-- | Append a value to a 'SafeList3'
appendSafeList3 :: a -> SafeList3 a LB.NonEmptyID -> SafeList3 a LB.NonEmptyID
appendSafeList3 appendMe (Cons x y z zs _) = Cons appendMe x y (z:zs) Nil

-- | Reverses the 'SafeList3'
reverseSafeList3 :: SafeList3 a LB.NonEmptyID -> SafeList3 a LB.NonEmptyID
reverseSafeList3 (Cons x y z zs _) =
  let
    reversedZS = reverse zs
    processReversed [x1,y1,z1] =
      let
        a = [z,y,x]
      in
      Cons x1 y1 z1 a Nil
    processReversed [x1,y1] =
      let
        a = [y,x]
      in
      Cons x1 y1 z a Nil
    processReversed [x1] =
      let
        a = (x:[])
      in
      Cons x1 z y [x] Nil
    processReversed [] =
      Cons z y x [] Nil
    processReversed list =
      let
        (a:b:c:cs) = (list) ++ [z,y,x]
      in
      Cons a b c cs Nil
  in
  processReversed reversedZS
  
  


type LineIdSafe3List = SafeList3 (Env.Id Env.LineInt) LB.NonEmptyID
type PointIdSafe3List = SafeList3 (Env.Id Env.PointInt) LB.NonEmptyID
type VertexSafe3List = SafeList3 V.Vertex LB.NonEmptyID


-- | For creating lists with guaranteed length >= 3, and other qualities depending on the contained type.
class ToSafeList3 a b | b -> a where
  toSafeList3 :: a -> Either Hex.HasMeshException b

instance LB.IsUnique VertexSafe3List where
  isUnique vertexSafe3List = 
   let
    checkUnique :: [V.Vertex] -> Bool
    checkUnique [] = True
    checkUnique [_] = True
    checkUnique (v:vs) =
      not (L.elem v vs) && checkUnique vs
   in
    checkUnique $ evalSafeList3 vertexSafe3List
  
instance ToSafeList3 [Env.Id Env.LineInt] LineIdSafe3List where
  toSafeList3 [] = Left $ Hex.SafeList3MinError "length == 0"
  toSafeList3 [_] = Left $ Hex.SafeList3MinError "length == 1"
  toSafeList3 [_,_] = Left $ Hex.SafeList3MinError "length == 2"
  toSafeList3 [x,y,z] = Right $ Cons x y z [] Nil
  toSafeList3 (x:y:z:zs) = Right $ Cons x y z zs Nil


instance ToSafeList3 [Env.Id Env.PointInt] PointIdSafe3List where  
  toSafeList3 [] = Left $ Hex.SafeList3MinError "length == 0"
  toSafeList3 [_] = Left $ Hex.SafeList3MinError "length == 1"
  toSafeList3 [_,_] = Left $ Hex.SafeList3MinError "length == 2"
  toSafeList3 [x,y,z] = Right $ Cons x y z [] Nil
  toSafeList3 (x:y:z:zs) = Right $ Cons x y z zs Nil

-- | Need to have guarantee that there will be no duplicate vertices. This will also take care of it being open.
instance ToSafeList3 [V.Vertex] VertexSafe3List where
  toSafeList3 [] = Left $ Hex.SafeList3MinError "length == 0"
  toSafeList3 [_] = Left $ Hex.SafeList3MinError "length == 1"
  toSafeList3 [_,_] = Left $ Hex.SafeList3MinError "length == 2"
  --toSafeList3 [x,y,z] = Right $ Cons x y z [] Nil
  toSafeList3 [x,y,z] =
    let
      theList = Cons x y z [] Nil
    in
    if isUnique theList then
      Right theList
    else
      Left $ Hex.NonUnique "non unique safe [Vertex]"
    
  --toSafeList3 (x:y:z:zs) = Right $ Cons x y z zs Nil
  toSafeList3 (x:y:z:zs) =
    let
      theList = Cons x y z zs Nil
    in
    if isUnique theList then
      Right theList
    else
      Left $ Hex.NonUnique "non unique safe [Vertex]"
      
instance LB.IsOpen PointIdSafe3List where
  isOpen pointIdSafe3List = safeHead3 pointIdSafe3List /= safeLast3 pointIdSafe3List
  
instance LB.IsOpen VertexSafe3List where
  isOpen vertexSafe3List = safeHead3 vertexSafe3List /= safeLast3 vertexSafe3List


{-  
-- | Ensure that the 'VertexSafe3List' has no duplicate values
-- Should stop exporting it, as is now implemented by the toSafeList fx. Is handy for testing though.
isUnique ::  VertexSafe3List -> Bool
isUnique safeList =
  let
    checkUnique :: [V.Vertex] -> Bool
    checkUnique [] = True
    checkUnique [_] = True
    checkUnique (v:vs) =
      not (L.elem v vs) && checkUnique vs
  in
    checkUnique $ evalSafeList3 safeList
-}
