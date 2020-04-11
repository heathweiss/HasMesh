{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
import qualified Utils.RunExceptions as HexR
-}
module Utils.RunExceptions(runEitherIO, runEitherRIO) where

import RIO
import qualified RIO.Text as T
import qualified Utils.Exceptions as Hex
import qualified Utils.Environment as Enviro
import qualified Utils.FileWriter as FW

-- | Used to check the status of an Either while in the IO monad. Eliminates the use of EitherT as recommended by RIO monad.
--
-- On Right: will extract the value to IO.
--
-- On Left:  throws an InvestPassThru exception with the Left msg appended to a location of where in the fucntion it happened.
-- The `catch' 'InvestPassThru' at the end of the function, will append the module and function name.

runEitherIO :: T.Text -> Either Hex.HasMeshException a -> IO (a)
runEitherIO _ (Right a) = return a
runEitherIO location (Left(Hex.ZeroLengthName msg)) = do
  throwIO $ Hex.ZeroLengthName $ location <> ": " <> msg

  

-- | Used to check the status of an Either while in the RIO monad. Eliminates the use of EitherT as recommended by RIO monad.
--
-- On Right: will extract the value to RIO.
--
-- On Left: throws an InvestPassThru exception with the Left msg appended to a location of where in the fucntion it happened.
-- The `catch' 'InvestPassThru' at the end of the function, will append the module and function name.
runEitherRIO :: T.Text -> Either Hex.HasMeshException a -> RIO env (a)
--runEitherRIO :: {-(Enviro.HasDesignName env,Enviro.HasPointIdSupply env, Enviro.HasLineIdSupply env) =>-} T.Text -> Either Hex.HasMeshException a -> RIO env (a)
--There should be no need for all these restrictions, as it is not using any of them. The calling code has it's own version of env, and does not use this env.
runEitherRIO _ (Right a) = return a
runEitherRIO location (Left(Hex.ZeroLengthName msg)) = do
  throwIO $ Hex.ZeroLengthName $ location <> ": " <> msg 


