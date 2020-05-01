{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Supplies a global Environment, as per FPComplete RIO recommendations.

This includes all ID functions. Eg: 'Id': 'PointInt' and 'LineInt'.

import qualified Utils.Environment as Env
-}
module Utils.Environment(Environment(..), PointIdStatus(..),  HasIdSupply(..), HasPointIdMap(..), HasGeoFileHandle(..), HasDesignName(..), HasScriptWriter(..),
                         toEnvironment, getPointId, Id(..), LineInt(), pattern LineInt', Initialize(..),   PointInt(), pattern PointInt',
                         pattern DesignNameP, loadLoader,
                         
                         evalLineId, evalPointId, getLineId, incr, newDesignName, designFilePath 
                        ) where



import qualified RIO.ByteString as B
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson
import RIO
import qualified Utils.Exceptions as Hex

import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
--import qualified Geometry.Geometry as Geo

import qualified Geometry.Vertex as V
--import qualified Utils.Design as Design



-- Environment.yaml read and parsed into a 'Loader' which is pre 'Environment' data. It will be used to create the 'Environment' data.
-- Loader is required as an intermediate step, as the yaml does not contain ID supplies, and the 'designName' needs to be validated.
newtype Loader = Loader
                  { designName :: Text   -- The DesignName. Used to build the path to the saved file.
                  } deriving (Show, Generic)
instance FromJSON Loader

-- | Read the Environment.yaml file, and parse the yaml into the 'Loader'.
loadLoader :: IO Loader
loadLoader = do
    content <- B.readFile "Environment.yaml" 
    let parsedContent = Y.decode content :: Maybe Loader 
    case parsedContent of
        Nothing -> error "Could not parse config file."
        
        (Just (Loader designName)) -> do
          return $ Loader designName

-- | Global 'Environment' for the RIO monad, which is used throughout HasMesh.
data Environment = 
  Env { env_designName :: !Text, -- ^ The 'DesignName'. Used to build the path to the saved file.
        env_pointIdSupply :: !(IORef (Id PointInt)), -- ^ The supply for 'Geometry.Gmsh.PointID'
        env_pointIdMap :: !(IORef (Map Int (Id PointInt))), -- ^ The map containing the 'Gmsh.GPointId's associated with each 'Geometry.Vertex.Vertex'. Used to ensure a 'Geometry.Vertex.Vertex' only has a single 'Id' 'PointInt'.
        env_geoFileHandle :: !(IORef Handle), -- ^ Handle for writing gmsh script to the design file. Set to stdout for default value.
        env_lineIdSupply :: !(IORef (Id LineInt)), -- ^ The supply for 'Geometry.Gmsh.PointID'
        env_pntScriptWriter :: Handle -> PointIdStatus -> V.Vertex -> IO (Id PointInt), -- ^ Function to write the Gmsh point to file, if required to do so.
        env_lineScriptWriter :: Handle -> Id LineInt -> Id PointInt -> Id PointInt -> IO (Id LineInt) -- ^ Function to write the Gmsh line to file, if required to do so.
      }
  

-- | Show the Environment for testing.
instance Show Environment where
  show (Env designName _ _ _ _ _ _) = show designName
  
  

-- | Convert the 'Loader', which loaded/decoded the Environment.yaml, into an 'Environment'
toEnvironment :: Loader -> IORef (Id PointInt) -> IORef (Map Int (Id PointInt)) -> IORef Handle -> IORef (Id LineInt)
              -> (Handle -> PointIdStatus -> V.Vertex -> IO (Id PointInt))
              -> (Handle -> Id LineInt -> Id PointInt -> Id PointInt -> IO (Id LineInt))
              -> Environment
toEnvironment (Loader designName') = Env designName'

-- | Supplies a Handle for writing gmsh script. This could be a file handle for a .geo file, or stdout.
class HasGeoFileHandle env where
  geoFileHandleL :: Lens' env (IORef Handle) -- ^ The Handle to the .geo design file.

instance HasGeoFileHandle Environment where
  geoFileHandleL = lens env_geoFileHandle (\x y -> x {env_geoFileHandle = y})

-- | The map in which gmsh points are stored, with the associated 'Geometry.Vertex.Vertex' as a key.
class HasPointIdMap env where
  pointIdMapL :: Lens' env (IORef (Map Int (Id PointInt))) -- ^ The map of 'Gmsh.PointId's associated with each 'Geometry.Vertex.Vertex'
instance HasPointIdMap Environment where
  pointIdMapL = lens env_pointIdMap (\x y -> x {env_pointIdMap = y})

class HasIdSupply env where
  pointIdSupplyL :: Lens' env (IORef (Id PointInt)) -- ^ Supply of 'Gmsh.PointId'
  lineIdSupplyL :: Lens' env (IORef (Id LineInt)) -- ^ Supply of 'Gmsh.LineId'

instance HasIdSupply Environment where
  pointIdSupplyL = lens env_pointIdSupply (\x y -> x {env_pointIdSupply = y})
  lineIdSupplyL = lens env_lineIdSupply (\x y -> x {env_lineIdSupply = y}) 

class HasDesignName env where
  designNameL :: Lens' env T.Text -- ^ 'DesignName'

instance HasDesignName Environment where
  designNameL = lens env_designName (\x y -> x {env_designName = y})

class HasScriptWriter env where
  pntScriptWriterL :: Lens' env (Handle -> PointIdStatus -> V.Vertex -> IO (Id PointInt)) -- ^ Write the gmsh script for points.
  lineScriptWriterL :: Lens' env (Handle -> Id LineInt -> Id PointInt -> Id PointInt -> IO (Id LineInt)) -- ^ Write the gmsh script for points.
  
  

instance HasScriptWriter Environment where
  pntScriptWriterL = lens env_pntScriptWriter (\x y -> x {env_pntScriptWriter = y})
  lineScriptWriterL = lens env_lineScriptWriter (\x y -> x {env_lineScriptWriter = y})
  

-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------- ID-----------------------------------------------------------------
-- | Wraps a point id with a status indicating if it is a new id, or if the vertex and id already existed.
-- This is used in decision to print the point gmsh script.
data PointIdStatus = PointIdAlreadyExisted (Id PointInt) |  PointIdDidNotExist (Id PointInt) deriving (Show,Eq)

-- | Get a Gmsh Id for a 'V.Vertex'. The 'PointIdStatus' is set according to if the point/vertex already exists.
getPointId :: (HasIdSupply env, HasPointIdMap env) =>  V.Vertex -> RIO env  PointIdStatus
getPointId doesThisVertexHaveAnId = do
  pointMapIORef <- view pointIdMapL
  pointMap <- readIORef pointMapIORef
  case Map.lookup (H.hash doesThisVertexHaveAnId) pointMap of
    Just pointId -> return $ PointIdAlreadyExisted pointId
    Nothing -> do
      pointIdSupply <- view pointIdSupplyL
      currPointId <- readIORef pointIdSupply
      writeIORef pointIdSupply (incr currPointId )
      writeIORef pointMapIORef $ Map.insert (H.hash doesThisVertexHaveAnId ) currPointId pointMap
      return $ PointIdDidNotExist  currPointId
      
      
    

-- Uses GADTs in order to implement the typecase pattern. See book: Haskell Design Patterns.
-- Using GADTs has not gained me anything, other than getting some experience with GADT's.
-- By having them all be of type Int, they can all use the incr fx.
-- | The gmsh Point ID that is associated with a 'V.Vertex' 
-- These are used by the 'ID' GADT to give a unique return type for each constructor.
newtype PointInt = PointInt Int deriving (Show,Eq)
pattern PointInt' :: Int -> PointInt
pattern PointInt' i <- PointInt i


-- | The gmsh Line ID that is associated with a pair of Gmsh points.
-- These are used by the 'ID' GADT to give a unique return type for each constructor.
newtype LineInt = LineInt Int deriving (Show,Eq)
pattern LineInt' :: Int -> LineInt
pattern LineInt' i <- LineInt i


-- | Identifiers that correspond to the IDs used in Gmsh script and APIs.
data Id id where
  PointId :: PointInt -> Id PointInt -- ^ An Gmsh ID for a 'Geometry.Vertex.Vertex'. Only 1 ID will exist for a given 'Geometry.Vertex.Vertex'.
  LineId  :: LineInt -> Id LineInt   -- ^ A Gmsh ID for a line associated with 2 'Geometry.Vertex.Vertex'. Uniqueness is not enforced.
  
deriving instance Show (Id a)
deriving instance  Eq (Id a)


-- | Increment an 'ID' < 'PointId' 'LineId' > by 1.
--
--  Used for supplying Gmsh ids for 'Geo.Vector' and and line ids for ['Geo.Vector']
incr :: Id a -> Id a
incr (PointId (PointInt int)) = PointId $ PointInt $ int + 1
incr (LineId  (LineInt int)) = LineId $ LineInt $ int + 1


-- | Extract the Int.
evalLineId :: Id LineInt-> Int
evalLineId (LineId (LineInt int)) = int
          
-- | Extract the Int.
evalPointId :: Id PointInt -> Int
evalPointId (PointId pointInt) = evalPointInt pointInt
evalPointInt :: PointInt -> Int
evalPointInt (PointInt int) = int


-- | Get the next available 'Env.Id Env.LineInt' that corresponds to a Gmsh line.
getLineId :: (HasIdSupply env) =>   RIO env (Id LineInt)
getLineId = do
  lineIdSupplyIORef <- view lineIdSupplyL
  lineIdSupply <- readIORef lineIdSupplyIORef
  writeIORef lineIdSupplyIORef $ incr lineIdSupply
  return lineIdSupply

class Initialize a where
  initialId :: Id a
  
  
instance Initialize PointInt where
  initialId = PointId $ PointInt 1
  
instance Initialize LineInt where
  initialId = LineId $ LineInt 1
  

-- | Name of the 3D design, used to build filepath for saving a design .geo file.
newtype DesignName = DesignName {validDesignName :: Text} deriving (Eq,Show)

pattern DesignNameP name  <- DesignName name





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



designFilePath :: DesignName ->  FilePath
designFilePath ( DesignName designName) = 
  T.unpack $ "src/Data/GeoFiles/" <> designName <> ".geo"
  



