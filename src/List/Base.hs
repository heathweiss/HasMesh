{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-
Supply base functionality common to List.Safe3 and List.Safe1

import qualified List.Base as LB
-}
module List.Base(Empty(..), NonEmptyID(..), IsOpen(..),) where

import RIO
import qualified RIO.List as L
import qualified Utils.Environment as Env
import qualified Utils.Exceptions as Hex
import qualified Geometry.Vertex as V
import Utils.Add

data Empty
deriving instance Show (Empty)

data NonEmptyID
deriving instance Show (NonEmptyID)


-- This needs to be moved into the Gmsh.Status module.
class IsOpen a where
  isOpen :: (Eq a) =>  a -> Bool
  
