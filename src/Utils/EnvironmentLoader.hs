{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
import qualified Utils.EnvironmentLoader as EnvLdr

Loads the 'Utils.Environment.Environment' from Environment.yaml. Reads the designName and reloads
it using ''Utils.FileWriter.newDesignName' to ensure it is valid. If it is not valid, an 'Utils.Exceptions.HaMeshException' is thrown.
This would happen if the Environment.yaml file has in invalid designName.
-}
module Utils.EnvironmentLoader(loadEnvironment) where

import RIO
import qualified RIO.Text as T
--import qualified Data.IORef as IOref

import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR
import qualified Utils.Environment as Enviro
import qualified Utils.FileWriter as FW
import qualified Geometry.ID as ID


-- | Load the global RIO 'Environment'
loadEnvironment :: IO (Enviro.Environment)
loadEnvironment = do
  loaded <- Enviro.loadLoader
  ioref <- newIORef $ ID.PointId 1 
  
  let
    env_ = Enviro.toEnvironment loaded ioref
  validDesignName <- HexR.runEitherIO "validDesignName" $ FW.newDesignName $  view FW.designNameL env_
  return $ env_ 


