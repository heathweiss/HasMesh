{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}


{-
Load 'Utils.Environment.Environment's.

Required because of all the cyclic imports created if everything is done from within Environment.

import qualified Utils.EnvironmentLoader as EnvLdr 
-}

module Utils.EnvironmentLoader(loadEnvironment, loadTestEnvironment) where


import RIO
import qualified RIO.Map as Map
import qualified Utils.RunExceptions as HexR

import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.Environment as Env

-- | Load the global RIO 'Environment'
-- Initializes all Id supplies with initial value of 1, and the 'env_pointIdMap' as an empty map.
--
-- Throws an error if the 'env_designName' is not valid.
loadEnvironment :: IO Env.Environment
loadEnvironment = do
  loaded <- Env.loadLoader
  iorefPointIdSupply <- newIORef Env.initialId
  iorefPoints <- newIORef $ Map.fromList []
  iorefDesignFileHandle <- newIORef stdout
  iorefLineIdSupply <- newIORef Env.initialId
  iorefCurveLoopIdSupply <- newIORef Env.initialId
  iorefPlaneSurfaceIdSupply <- newIORef Env.initialId
  HexR.runEitherIO
    "validDesignName"
    $ Env.toEnvironment loaded iorefPointIdSupply iorefPoints iorefDesignFileHandle iorefLineIdSupply
                       ScrB.pointWriter ScrB.lineWriter iorefCurveLoopIdSupply ScrB.curveLoopWriter iorefPlaneSurfaceIdSupply
                       ScrB.planeSurfaceWriter
                       
  
-- | Load a global RIO 'Environment.Environment' for testing. Set values to default values that will not change.
-- Handle is set to stdout. The points, lines, etc do not get printed out as script.
-- Set the Handle to stdout.
loadTestEnvironment :: IO Env.Environment
loadTestEnvironment = do
  loaded <- Env.loadLoader
  iorefPointIdSupply <- newIORef Env.initialId
  iorefPoints <- newIORef $ Map.fromList []    
  iorefDesignFileHandle <- newIORef stdout
  iorefLineIdSupply <- newIORef Env.initialId
  iorefCurveLoopIdSupply <- newIORef Env.initialId
  iorefPlaneSurfaceIdSupply <- newIORef Env.initialId
  HexR.runEitherIO
    "validDesignName" 
    $ Env.toEnvironment loaded iorefPointIdSupply iorefPoints iorefDesignFileHandle iorefLineIdSupply
                        ScrB.nullPointWriter ScrB.nullLineWriter iorefCurveLoopIdSupply ScrB.nullCurveLoopWriter iorefPlaneSurfaceIdSupply
                        ScrB.nullPlaneSurfaceWriter
                        
                        
    
    

