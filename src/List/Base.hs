{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-
Supply base functionality common to List.Safe3 and List.Safe1

import qualified List.Base as LB
-}
module List.Base(Empty(), NonEmptyID(), IsOpen(..),) where

import RIO


data Empty
deriving instance Show Empty

data NonEmptyID
deriving instance Show NonEmptyID


-- Should be able to delete this, as it has been replaced by 
class IsOpen a where
  isOpen :: (Eq a) =>  a -> Bool
  
