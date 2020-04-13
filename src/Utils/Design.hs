{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Supply functionality for dealing with design files, as in file name, file paths.

import qualified Utils.Design as Design
-}
module Utils.Design(DesignName(..), newDesignName, designFilePath) where

import RIO
import qualified RIO.Text as T
import qualified Utils.Exceptions as Hex
import qualified Utils.Environment as Enviro

{- |
Create a new 'DesignName'
Throws an IO ('ZeroLengthName') exception if zero length, and so has to be used in the IO monad, which is where Env will usually be loaded.
Should look at using Control.Monad.Catch.MonadThrow as per M. Snoyman suggestion. See https://www.fpcomplete.com/blog/2017/07/the-rio-monad
-}
newDesignName  :: Text -> Either Hex.HasMeshException DesignName
newDesignName designName =
 case T.length designName == 0 of
   True -> Left $ Hex.ZeroLengthName "Zero length designName"
   False -> pure $ DesignName designName


-- | Name of the 3D design, used to build filepath for saving a design .geo file.
data DesignName = DesignName {validDesignName :: Text}

designFilePath :: DesignName ->  FilePath
designFilePath ( DesignName designName) = 
  T.unpack $ "src/Data/GeoFiles/" <> designName <> ".geo"
  

