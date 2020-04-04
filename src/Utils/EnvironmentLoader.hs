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
module Utils.EnvironmentLoader(loadEnvironment, loadTestEnvironment) where

import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map

import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR
import qualified Utils.Environment as Enviro
import qualified Utils.FileWriter as FW
import qualified Gmsh.Gmsh as Gmsh


-- | Load the global RIO 'Environment'
loadEnvironment :: IO (Enviro.Environment)
loadEnvironment = do
  loaded <- Enviro.loadLoader
  iorefPointIdSupply <- newIORef $ Gmsh.PointId 1
  iorefPoints <- newIORef $ Map.fromList []
  iorefDesignFileHandle <- newIORef stdout
  let
    env_ = Enviro.toEnvironment loaded iorefPointIdSupply iorefPoints iorefDesignFileHandle
  validDesignName <- HexR.runEitherIO "validDesignName" $ FW.newDesignName $  view FW.designNameL env_
  return $ env_ 


-- | Load the global RIO 'Environment', and set values to default values that wil not change. For testing.
loadTestEnvironment :: IO (Enviro.Environment)
loadTestEnvironment = do
  loaded <- Enviro.loadLoader
  iorefPointIdSupply <- newIORef $ Gmsh.PointId 1
  iorefPoints <- newIORef $ Map.fromList []
  iorefDesignFileHandle <- newIORef stdout
  let
    env_ = Enviro.toEnvironment loaded iorefPointIdSupply iorefPoints iorefDesignFileHandle
  return $ env_ {Enviro.env_designName = "testDesignName"} 


