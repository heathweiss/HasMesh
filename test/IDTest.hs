{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs #-}
module IDTest(runTests) where
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO

import qualified Geometry.Vertex as V
import qualified Geometry.Polar as Polar
import qualified Geometry.Axis as Axis
import qualified Gmsh.Gmsh as Gmsh
import qualified Geometry.Geometry as Geo
import qualified Utils.EnvironmentLoader as EnvLdr
import qualified Utils.Environment as Enviro
import qualified Utils.List as L
import qualified Utils.Exceptions as Hex
import qualified Gmsh.ID as ID
import qualified Gmsh.IDNew as IDNew





data PointIdStatus = PointIdAlreadyExisted (ID.Id ID.PointInt) |  PointIdDidNotExist (ID.Id ID.PointInt) deriving (Eq, Show)


runTests :: IO ()
runTests = do
 P.putStrLn "=============== IDTest ====================="
 

 -- =================================== pull the id's from an IORef using Gmsh.incr ===================
-- will need to move this to ID 


 --Get a new pointId, and see that the ioref id supply was updated
{- leave till all ID functionality is moved into Enviro.
 let
  getANewPointId = TestCase
   (do 
      env <- EnvLdr.loadTestEnvironment
      status <- runRIO env $ IDNew.newPointId $ V.newVertex 2 2 2
      
      assertEqual "get the vector id from an ioref" ID.initialId status
   )
 _ <- runTestTT getANewPointId
-}
 let
  getANewHardCodedPointIdFromEviro = TestCase
   (do 
      env <- EnvLdr.loadTestEnvironment
      newStatusPointId <- runRIO env $ Enviro.getPointId $ V.newVertex 2 2 2
      assertEqual "the status and id are hardcoded in" (Enviro.PointIdDidNotExist(ID.initialId)) newStatusPointId
   )
 _ <- runTestTT getANewHardCodedPointIdFromEviro

 let
  getASecondPointIdFromEnviro = TestCase
   (do 
      env <- EnvLdr.loadTestEnvironment
      _ <- runRIO env $ Enviro.getPointId $ V.newVertex 2 2 2
      secondStatusPointId <- runRIO env $ Enviro.getPointId $ V.newVertex 2 2 2
      assertEqual "the status and id are hardcoded in" (Enviro.PointIdDidNotExist(ID.incr $ ID.initialId)) secondStatusPointId
      
   )
 _ <- runTestTT getASecondPointIdFromEnviro

 let
  getAThirdPointIdFromEnviro = TestCase
   (do 
      env <- EnvLdr.loadTestEnvironment
      _ <- runRIO env $ Enviro.getPointId $ V.newVertex 1 1 1
      _ <- runRIO env $ Enviro.getPointId $ V.newVertex 2 2 2
      thirdStatusPointId <- runRIO env $ Enviro.getPointId $ V.newVertex 3 3 3
      assertEqual "the status and id are hardcoded in" (Enviro.PointIdDidNotExist(ID.incr $ ID.incr $ ID.initialId)) thirdStatusPointId
      
   )
 _ <- runTestTT getAThirdPointIdFromEnviro

 


 P.putStrLn "" 

