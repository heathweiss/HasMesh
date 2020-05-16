{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-
Supply base functionality common to List.Safe3 and List.Safe1

import qualified List.Base as LB
-}
module List.Base(Empty(), NonEmptyID(), IsOpen(..),IsUnique(..), Add(..)) where

import RIO
import qualified Utils.Exceptions as Hex

-- | ADT required for building safe lists.
data Empty
deriving instance Show Empty

-- | ADT required for building safe lists.
data NonEmptyID
deriving instance Show NonEmptyID


-- | Should be able to delete this, as it has been replaced by IsUnique
class IsOpen a where
  isOpen :: (Eq a) =>  a -> Bool

-- | Used for lists in which duplicates values cannot exist.
class IsUnique a where
  isUnique ::  a -> Bool

-- | Add types where the addition can result in an exception. Is like >>= specialized to addition.
--
-- So far, only used for adding safe lists.
class Add a where
  
  (>>+) :: (IsUnique a) =>  Either Hex.HasMeshException a -> a -> Either Hex.HasMeshException a
