{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
import qualified Utils.Environment as Enviro
-}
module Utils.Environment(Environment(..),  HasPointIdSupply(..), HasPointIdMap(..), loadLoader, toEnvironment,) where

import qualified RIO.ByteString as B
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson

import RIO
import qualified Prelude as P
import qualified RIO.Text as T

import qualified Gmsh.Gmsh as Gmsh
--import qualified Geometry.ID as ID
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
        env_pointIdSupply :: !(IORef Gmsh.PointId), -- ^ The supply for 'Geometry.Gmsh.PointID'
        env_pointIdMap :: !(IORef (Map Int Gmsh.PointId)) -- ^ The map containing the 'Gmsh.GPointId's associated with each 'Geometry.Vertex.Vertex'. The key is a hash of the Vertex. todo: Wrap vertex in an ADT?
      } 

-- | Show the Environment for testing. Can't show the IORef.
instance Show Environment where
  show (Env designName _ _) = show designName

--convert a Loader to an Environment.
--Needs to load in an IORef from an IO monad, so must be called from IO
toEnvironment :: Loader -> IORef Gmsh.PointId -> IORef (Map Int Gmsh.PointId) -> Environment
toEnvironment (Loader designName ) iorefPointIdSupply iorefPoints =
  Env designName iorefPointIdSupply iorefPoints 

class HasPointIdMap env where
  env_pointIdMapL :: Lens' env (IORef (Map Int Gmsh.PointId)) -- ^ The map of 'Gmsh.PointId's associated with each 'Geometry.Vertex.Vertex'


instance HasPointIdMap Environment where
  env_pointIdMapL = lens env_pointIdMap (\x y -> x {env_pointIdMap = y})


class HasPointIdSupply env where
  env_pointIdSupplyL :: Lens' env (IORef Gmsh.PointId) -- ^ Supply of 'Gmsh.PointId'


instance HasPointIdSupply Environment where
  env_pointIdSupplyL = lens env_pointIdSupply (\x y -> x {env_pointIdSupply = y}) 


