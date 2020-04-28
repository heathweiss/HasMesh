{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs #-}
module EnvironmentTest(runTests) where
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO
import qualified Geometry.Vertex as V
import qualified Gmsh.Gmsh as Gmsh
import qualified Utils.Environment as Env
import qualified Utils.Exceptions as Hex 

runTests :: IO ()
runTests = do
 P.putStrLn "=============== Environment ====================="

 --------------------------------------------------------------------------------------------------------------------
 --------------------------------------------------------------------------------------------------------------------
 --------------------------------------------------------------------------------------------------------------------
 -- Create Points from Vertex, and look at their status: PointIdDidNotExist vs PointIdAlreadyExisted
 let
  getStatusOfANewVertex = TestCase
   (do 
      env <- Env.loadTestEnvironment
      newStatusPointId <- runRIO env $ Env.getPointId $ V.newVertex 2 2 2
      assertEqual "a new vertex gives PointIdDidNotExist" (Env.PointIdDidNotExist(Env.initialId)) newStatusPointId
   )
 _ <- runTestTT getStatusOfANewVertex

 let
  getStatusOfAnAlreadyExistingVertex = TestCase
   (do 
      env <- Env.loadTestEnvironment
      _ <- runRIO env $ Env.getPointId $ V.newVertex 2 2 2
      secondStatusPointId <- runRIO env $ Env.getPointId $ V.newVertex 2 2 2
      assertEqual "an existing vertex gives PointIdAlreadyExisted" (Env.PointIdAlreadyExisted(Env.initialId)) secondStatusPointId
      
   )
 _ <- runTestTT getStatusOfAnAlreadyExistingVertex

 let
  getStatusOfASecondNewVertex = TestCase
   (do 
      env <- Env.loadTestEnvironment
      _ <- runRIO env $ Env.getPointId $ V.newVertex 1 1 1
      _ <- runRIO env $ Env.getPointId $ V.newVertex 2 2 2
      thirdStatusPointId <- runRIO env $ Env.getPointId $ V.newVertex 3 3 3
      assertEqual "a second new vertex gives PointIdDidNotExist" (Env.PointIdDidNotExist(Env.incr $ Env.incr $ Env.initialId)) thirdStatusPointId
      
   )
 _ <- runTestTT getStatusOfASecondNewVertex

 --------------------------------------------------------------------------------------------------------------------
 --------------------------------------------------------------------------------------------------------------------
 --------------------------------------------------------------------------------------------------------------------
 -- validate a design name
 let
  createValidDesignName = TestCase $ assertEqual
   "create a valid design name"
   (Right "testname")
   (let
       name = Env.newDesignName "testname"
    in
      case name of
        (Right (Env.DesignNameP name)) -> Right name
        Left (Hex.ZeroLengthName msg) -> Left (Hex.ZeroLengthName msg)
   )
 _ <- runTestTT createValidDesignName
 

 let
  createInvalidDesignName = TestCase $ assertEqual
   "create an invalid design name"
   (Left (Hex.ZeroLengthName "Zero length designName"))
   (let
       name = Env.newDesignName ""
    in
      case name of
        (Right (Env.DesignNameP name)) -> Right name
        Left (Hex.ZeroLengthName msg) -> Left (Hex.ZeroLengthName msg)
   )
 _ <- runTestTT createInvalidDesignName
 


 P.putStrLn ""
 

