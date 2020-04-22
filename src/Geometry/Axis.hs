{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Supply the X Y Z axis for the cartesian plane.

import qualified Geometry.Axis as Axis
-}
module Geometry.Axis(XAxis(..), YAxis(..), ZAxis(..),) where

import Import
import qualified RIO.Text as T



newtype XAxis = XAxis {_xAxis :: Double} deriving (Eq, Typeable, Data)
newtype YAxis = YAxis {_yAxis :: Double} deriving (Eq, Typeable, Data)
newtype ZAxis = ZAxis {_zAxis :: Double} deriving (Eq, Typeable, Data)



instance Show XAxis where
  show (XAxis xAxis) = T.unpack $ tshow xAxis
  
instance Show YAxis where
  show (YAxis yAxis) = T.unpack $ tshow yAxis

instance Show ZAxis where
  show (ZAxis zAxis) = T.unpack $  tshow zAxis
  
  
{-
instance Show XAxis where
  show (XAxis xAxis) = T.unpack $ "x: "  <> tshow xAxis

instance Show YAxis where
  show (YAxis yAxis) = T.unpack $ "y: "  <> tshow yAxis

instance Show ZAxis where
  show (ZAxis zAxis) = T.unpack $ "z: "  <> tshow zAxis

-}  
  
  
  
  
  
  
