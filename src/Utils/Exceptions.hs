{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Supply custom exceptions.

Supply conversions from Either to exceptions. Might get rid of this in favor of MonadThrow.

import qualified Utils.Exceptions as Hex 
-}
module Utils.Exceptions(HasMeshException(..)) where
import RIO
import qualified Prelude as P
import qualified RIO.Text as T
import qualified Data.IORef as IOref

-- | Caught by all functions which use the runEither functions, or call a function which in turn uses it.
data HasMeshException = ZeroLengthName Text -- ^ For any Text or ByteString, for which the length must be > 0.
                     | GeneralException Text -- ^ Catch-all HasMesh Exception.
                     | SafeList3MinError Text -- ^ 'Gmsh.PointIdList must have length >= 3
  deriving (Typeable, Show, Eq)


instance Exception HasMeshException where
  displayException (GeneralException msg) = show msg
  displayException (ZeroLengthName msg) = "ZeroLengthName" ++ show msg 
  displayException (SafeList3MinError msg) = "SafeList3MinError with length < 3"

