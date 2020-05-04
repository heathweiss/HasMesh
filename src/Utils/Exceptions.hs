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
                     | SafeList1MinError Text -- ^ 'Utils.List.SafeList1' must have length >= 1
                     | SafeList3MinError Text -- ^ 'Utils.List.SafeList3' must have length >= 3
                     | PointIdSafe3ListIsClosed Text -- ^ 'Gmsh.PointIdList' head and last must not be equal
                     | NonUniqueVertex Text -- ^ 'Utils.List.VertexSafe3List' must be a ['Geometry.Vertex.Vertex'] without duplicate entries.
  deriving (Typeable, Show, Eq)


instance Exception HasMeshException where
  displayException (GeneralException msg) = show msg
  displayException (ZeroLengthName msg) = "ZeroLengthName" ++ show msg 
  displayException (SafeList3MinError msg) = "SafeList3MinError with length < 3"
  displayException (SafeList1MinError msg) = "SafeList1MinError with length < 1"
  displayException (PointIdSafe3ListIsClosed msg) = "Gmsh.PointIdList is not open. head and last must not be equal"
  displayException (NonUniqueVertex msg) = "'Utils.List.VertexSafe3List' must be a ['Geometry.Vertex.Vertex'] without duplicate entries."

