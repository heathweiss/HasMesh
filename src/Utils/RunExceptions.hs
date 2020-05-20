{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
import qualified Utils.RunExceptions as HexR
-}
module Utils.RunExceptions(runEither, runEitherIO, runEitherRIO) where

import RIO
import qualified RIO.Text as T
import qualified Utils.Exceptions as Hex
--import qualified Utils.Environment as Enviro
--import qualified Utils.Designs as Design
import qualified Control.Exception as Ex

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
runEitherIO location _ = do
  throwIO $ Hex.GeneralException $ location <> ": " <> "had an unhandled IO HasMeshException"
  

-- | Used to check the status of an Either while in the RIO monad. Eliminates the use of EitherT as recommended by RIO monad.
--
-- On Right: will extract the value to RIO.
--
-- On Left: throws an InvestPassThru exception with the Left msg appended to a location of where in the fucntion it happened.
-- The `catch' 'InvestPassThru' at the end of the function, will append the module and function name.
runEitherRIO :: T.Text -> Either Hex.HasMeshException a -> RIO env (a)
runEitherRIO _ (Right a) = return a
runEitherRIO location (Left(Hex.ZeroLengthName msg)) = do
  throwM $ Hex.ZeroLengthName $ location <> ": " <> msg
runEitherRIO location (Left(Hex.SafeList3MinError msg)) = do
  throwM $ Hex.SafeList3MinError $ location <> ": " <> msg 
runEitherRIO location _ = do
  throwIO $ Hex.GeneralException $ location <> ": " <> "had an unhandled RIO HasMeshException"

-- | If Left, throw an exception in pure code, otherwise just return the value.
runEither :: T.Text -> Either Hex.HasMeshException a -> a
runEither _ (Right a) = a
runEither location (Left (Hex.SafeList3MinError msg)) = Ex.throw $ Hex.SafeList3MinError $ location <> ": " <> msg


