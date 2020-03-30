{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Supply custom exceptions.

Supply conversions from Either to exceptions. Might get rid of this in favor of MonadThrow.

import qualified Exceptions as Utils.Hex
-}
module Utils.Exceptions(HasMeshException(..)) where
import RIO
import qualified Prelude as P
import qualified RIO.Text as T
import qualified Data.IORef as IOref


data HasMeshException = ZeroLengthName Text -- ^ Caught by all functions which use the runEither functions, or call a function which in turn uses it.
                     | GeneralException Text
  deriving (Typeable, Show)


instance Exception HasMeshException where
  displayException (GeneralException msg) = show msg
  displayException (ZeroLengthName msg) = "ZeroLengthName" ++ show msg 


