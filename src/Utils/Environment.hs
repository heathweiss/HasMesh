{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
import qualified Utils.Environment as Enviro
-}
module Utils.Environment(Environment(..), Loader(),  HasIdSupply(..), HasPointIdMap(..), HasGeoFileHandle(..), HasDesignName(..),  loadLoader, toEnvironment,) where

import qualified RIO.ByteString as B
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson
--import qualified System.IO as SIO
import RIO
import qualified Prelude as P
import qualified RIO.Text as T

import qualified Gmsh.ID as ID
--import qualified Geometry.ID as ID
--



-- |  Environment.yaml read and parsed into a 'Loader' which is pre 'Environment' data. It will be used to create the 'Environment' data.
-- So far it only loads the designName, which needs to be validated into a 'Utils.FileWriter.DesignName'  using 'Utils.FileWriter.newDesignName'
data Loader =
  Loader
      { designName :: Text   -- The DesignName. Used to build the path to the saved file.
        
      } deriving (Show, Generic)

instance FromJSON Loader 

-- | Read the Environment.yaml yaml file into the 'Loader'
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
        --env_pointIdSupply :: !(IORef (ID.Id Int)), -- ^ The supply for 'Geometry.Gmsh.PointID'
        env_pointIdSupply :: !(IORef (ID.Id ID.PointInt)), -- ^ The supply for 'Geometry.Gmsh.PointID'
        --env_pointIdMap :: !(IORef (Map Int (ID.Id Int))), -- ^ The map containing the 'Gmsh.GPointId's associated with each 'Geometry.Vertex.Vertex'. The key is a hash of the Vertex. todo: Wrap vertex in an ADT?
        env_pointIdMap :: !(IORef (Map Int (ID.Id ID.PointInt))), -- ^ The map containing the 'Gmsh.GPointId's associated with each 'Geometry.Vertex.Vertex'. The key is a hash of the Vertex. todo: Wrap vertex in an ADT?
        env_geoFileHandle :: !(IORef Handle), -- ^ Handle for writing gmsh script to the design file. Set to stdout for default value.
        env_lineIdSupply :: !(IORef (ID.Id ID.LineInt)) -- ^ The supply for 'Geometry.Gmsh.PointID'
      } 

-- | Show the Environment for testing. Can't show the IORef.
instance Show Environment where
  show (Env designName _ _ _ _) = show designName

--convert a Loader to an Environment.
--Needs to load in an IORef from an IO monad, so must be called from IO
toEnvironment :: Loader -> IORef (ID.Id ID.PointInt) -> IORef (Map Int (ID.Id ID.PointInt)) -> IORef Handle -> IORef (ID.Id ID.LineInt) -> Environment
toEnvironment (Loader designName ) iorefPointIdSupply iorefPoints iorefDesignFileHandle iorefLineIdSupply =
  Env designName iorefPointIdSupply iorefPoints iorefDesignFileHandle iorefLineIdSupply

class HasGeoFileHandle env where
  geoFileHandleL :: Lens' env (IORef Handle) -- ^ The Handle to the .geo design file.


instance HasGeoFileHandle Environment where
  geoFileHandleL = lens env_geoFileHandle (\x y -> x {env_geoFileHandle = y})


class HasPointIdMap env where
  pointIdMapL :: Lens' env (IORef (Map Int (ID.Id ID.PointInt))) -- ^ The map of 'Gmsh.PointId's associated with each 'Geometry.Vertex.Vertex'
instance HasPointIdMap Environment where
  pointIdMapL = lens env_pointIdMap (\x y -> x {env_pointIdMap = y})

class HasIdSupply env where
  pointIdSupplyL :: Lens' env (IORef (ID.Id ID.PointInt)) -- ^ Supply of 'Gmsh.ID.PointId'
  lineIdSupplyL :: Lens' env (IORef (ID.Id ID.LineInt)) -- ^ Supply of 'Gmsh.ID.LineId'

instance HasIdSupply Environment where
  pointIdSupplyL = lens env_pointIdSupply (\x y -> x {env_pointIdSupply = y})
  lineIdSupplyL = lens env_lineIdSupply (\x y -> x {env_lineIdSupply = y}) 

class HasDesignName env where
  designNameL :: Lens' env T.Text -- ^ 'DesignName'

instance HasDesignName Environment where
  designNameL = lens env_designName (\x y -> x {env_designName = y})
{-
class HasLineIdSupply env where
  lineIdSupplyL :: Lens' env (IORef (ID.Id ID.LineInt)) -- ^ Supply of 'Gmsh.ID.LineId'

instance HasLineIdSupply Environment where
  lineIdSupplyL = lens env_lineIdSupply (\x y -> x {env_lineIdSupply = y}) 
-}
{-
{- |
import qualified Utils.Environment as Enviro
-}
module Utils.Environment(Environment(..),  HasPointIdSupply(..), HasPointIdMap(..), HasGeoFileHandle(..), HasDesignName(..), HasLineIdSupply(..), loadLoader, toEnvironment,) where

import qualified RIO.ByteString as B
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson
--import qualified System.IO as SIO
import RIO
import qualified Prelude as P
import qualified RIO.Text as T

import qualified Gmsh.ID as ID
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
        --env_pointIdSupply :: !(IORef (ID.Id Int)), -- ^ The supply for 'Geometry.Gmsh.PointID'
        env_pointIdSupply :: !(IORef (ID.Id ID.PointInt)), -- ^ The supply for 'Geometry.Gmsh.PointID'
        --env_pointIdMap :: !(IORef (Map Int (ID.Id Int))), -- ^ The map containing the 'Gmsh.GPointId's associated with each 'Geometry.Vertex.Vertex'. The key is a hash of the Vertex. todo: Wrap vertex in an ADT?
        env_pointIdMap :: !(IORef (Map Int (ID.Id ID.PointInt))), -- ^ The map containing the 'Gmsh.GPointId's associated with each 'Geometry.Vertex.Vertex'. The key is a hash of the Vertex. todo: Wrap vertex in an ADT?
        env_geoFileHandle :: !(IORef Handle), -- ^ Handle for writing gmsh script to the design file. Set to stdout for default value.
        env_lineIdSupply :: !(IORef (ID.Id ID.LineInt)) -- ^ The supply for 'Geometry.Gmsh.PointID'
      } 

-- | Show the Environment for testing. Can't show the IORef.
instance Show Environment where
  show (Env designName _ _ _ _) = show designName

--convert a Loader to an Environment.
--Needs to load in an IORef from an IO monad, so must be called from IO
toEnvironment :: Loader -> IORef (ID.Id ID.PointInt) -> IORef (Map Int (ID.Id ID.PointInt)) -> IORef Handle -> IORef (ID.Id ID.LineInt) -> Environment
toEnvironment (Loader designName ) iorefPointIdSupply iorefPoints iorefDesignFileHandle iorefLineIdSupply =
  Env designName iorefPointIdSupply iorefPoints iorefDesignFileHandle iorefLineIdSupply

class HasGeoFileHandle env where
  geoFileHandleL :: Lens' env (IORef Handle) -- ^ The Handle to the .geo design file.


instance HasGeoFileHandle Environment where
  geoFileHandleL = lens env_geoFileHandle (\x y -> x {env_geoFileHandle = y})


class HasPointIdMap env where
  pointIdMapL :: Lens' env (IORef (Map Int (ID.Id ID.PointInt))) -- ^ The map of 'Gmsh.PointId's associated with each 'Geometry.Vertex.Vertex'
instance HasPointIdMap Environment where
  pointIdMapL = lens env_pointIdMap (\x y -> x {env_pointIdMap = y})

class HasPointIdSupply env where
  pointIdSupplyL :: Lens' env (IORef (ID.Id ID.PointInt)) -- ^ Supply of 'Gmsh.ID.PointId'
{-Version prior to GADT.
class HasPointIdSupply env where
  env_pointIdSupplyL :: Lens' env (IORef Gmsh.PointId) -- ^ Supply of 'Gmsh.PointId'
-}
instance HasPointIdSupply Environment where
  pointIdSupplyL = lens env_pointIdSupply (\x y -> x {env_pointIdSupply = y}) 

class HasDesignName env where
  designNameL :: Lens' env T.Text -- ^ 'DesignName'

instance HasDesignName Environment where
  designNameL = lens env_designName (\x y -> x {env_designName = y})

class HasLineIdSupply env where
  lineIdSupplyL :: Lens' env (IORef (ID.Id ID.LineInt)) -- ^ Supply of 'Gmsh.ID.LineId'

instance HasLineIdSupply Environment where
  lineIdSupplyL = lens env_lineIdSupply (\x y -> x {env_lineIdSupply = y}) 


-}
