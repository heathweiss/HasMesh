{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}

module Utils.EnvironmentLoader(loadEnvironment) where

import RIO
import qualified RIO.Text as T
import qualified Data.IORef as IOref

import qualified Utils.Exceptions as Hex
import qualified Utils.Environment as Enviro
import qualified Utils.FileWriter as FW
import qualified Geometry.ID as ID


-- | Load the global RIO 'Environment'
loadEnvironment :: IO (Enviro.Environment)
loadEnvironment = do
  loaded <- Enviro.loadLoader
  ioref <- IOref.newIORef $ ID.PointId 1 
  
  let
    env_ = Enviro.toEnvironment loaded ioref
  validDesignName <- FW.newDesignName $  view FW.designNameL env_
  
  return $ env_ 
