{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
import qualified Utils.RunExceptions as HexR
-}
module Utils.RunExceptions(runEitherIO, runEitherRIO,{- runEitherIOEx, runEitherRIOEx-}) where

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
{-
runEitherIO :: T.Text -> Either Text a -> IO (a)
runEitherIO _ (Right a) = return a
runEitherIO location (Left msg) = do
  throwIO $ Hex.GeneralException $ location <> msg
-}

runEitherIO :: T.Text -> Either Hex.HaMeshException a -> IO (a)
runEitherIO _ (Right a) = return a
runEitherIO location (Left(Hex.ZeroLengthName msg)) = do
  throwIO $ Hex.ZeroLengthName $ location <> ": " <> msg

  

-- | Used to check the status of an Either while in the RIO monad. Eliminates the use of EitherT as recommended by RIO monad.
--
-- On Right: will extract the value to RIO.
--
-- On Left: throws an InvestPassThru exception with the Left msg appended to a location of where in the fucntion it happened.
-- The `catch' 'InvestPassThru' at the end of the function, will append the module and function name.
{-
runEitherRIO :: (FW.HasDesignName env,Enviro.HasPointId env) => T.Text -> Either Text a -> RIO env (a)
runEitherRIO _ (Right a) = return a
runEitherRIO location (Left msg) = do
  throwIO $ Hex.GeneralException $ location <> msg 
-}
runEitherRIO :: (FW.HasDesignName env,Enviro.HasPointId env) => T.Text -> Either Hex.HaMeshException a -> RIO env (a)
runEitherRIO _ (Right a) = return a
runEitherRIO location (Left(Hex.ZeroLengthName msg)) = do
  throwIO $ Hex.ZeroLengthName $ location <> ": " <> msg 
