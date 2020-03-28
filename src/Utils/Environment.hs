{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
import qualified Environment as Enviro
-}
module Utils.Environment(Environment(..),  HasPointId(..), loadLoader, toEnvironment,) where

import qualified RIO.ByteString as B
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson

import RIO
import qualified Prelude as P
import qualified RIO.Text as T
import qualified Data.IORef as IOref

import qualified Geometry.ID as ID
--



--  The Env as read from Loader.yaml.
data Loader =
  Loader
      { designName :: Text   -- The DesignName. Used to build the path to the saved file.
        
      } deriving (Show, Generic)

instance FromJSON Loader 

loadLoader :: IO (Loader)
loadLoader = do
    content <- B.readFile "Environment.yaml" 
    let parsedContent = Y.decode content :: Maybe Loader 
    case parsedContent of
        Nothing -> error "Could not parse config file."
        
        (Just (Loader designName)) -> do
          return $ Loader designName

-- | Environment to the RIO monad, which is used throughout HasMesh.
data Environment = 
  Env { env_designName :: !Text, -- ^ The 'DesignName'. Used to build the path to the saved file.
        env_pointId :: !(IOref.IORef ID.PointId) -- ^ The supply for 'Geometry.ID.PointID'
        
      } 

-- | Show the Environment for testing. Can't show the IORef.
instance Show Environment where
  show (Env designName _ ) = show designName

--convert a Loader to an Environment.
--Needs to load in an IORef from an IO monad, so must be called from IO
toEnvironment :: Loader -> IOref.IORef ID.PointId -> Environment
toEnvironment (Loader designName ) ioref =
  Env designName ioref -- 







class HasPointId env where
  env_pointIdL :: Lens' env (IOref.IORef ID.PointId) -- ^ Supply of 'ID.PointId'


instance HasPointId Environment where
  env_pointIdL = lens env_pointId (\x y -> x {env_pointId = y}) 
