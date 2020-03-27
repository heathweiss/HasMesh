{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Testing() where

import Import
import Run
import RIO.Process
import qualified Paths_HasMesh

import qualified Prelude as P

import qualified Geometry.ID as ID

showVector = P.putStrLn $ show $ ID.incr $ ID.PointId 1
