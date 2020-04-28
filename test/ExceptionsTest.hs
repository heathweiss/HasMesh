{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module ExceptionsTest(runTests) where

import RIO
import Test.HUnit
import qualified Prelude as P

import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR
import qualified Utils.Environment as Env

runTests = do
 P.putStrLn $ "=============== Exceptions Test ====================="  
-- =============================  ==========================================

 --extract value in IO
 let
  testExtractEitherExceptionIO = TestCase
   (do
      let
        workInRIO :: RIO env (Either Hex.HasMeshException Bool)
        workInRIO = do
          return $ Right True
      env <- Env.loadTestEnvironment
      eitherResult <- runRIO env workInRIO
      result <- HexR.runEitherIO "result" eitherResult
      assertEqual "extract a Bool from a Either Hex.HasMeshException Bool" (True) result 
   )
 runTestTT testExtractEitherExceptionIO


 
 --Compose RIO monads with >>= and the HexR.runEitherRIO fx,
 let
  testExtractEitherExceptionRIO = TestCase
   (do
      
      let
        
        supplyBool :: Bool -> RIO env (Either Hex.HasMeshException Bool)
        supplyBool bool = do
          return $ Right bool

        consumeAndCompare :: (Bool) -> RIO env (Either Hex.HasMeshException Bool)
        consumeAndCompare bool = do
          return $ Right $ bool && True
         
        runRioComposition :: RIO env (Bool)
        runRioComposition = do
          env <- ask
          -- ========================= composition ==============================
          -- 
          bool <- runRIO env $ supplyBool True >>= HexR.runEitherRIO "supply" >>= consumeAndCompare  >>=  HexR.runEitherRIO "consumed"
          --note that return value is no longer in an Either.
          --If and error had occured at any point, would have exited via a throwIO from runEitherRIO
          --Could have left it in an Either, but not including the final >>=  HexR.runEitherRIO "consumed"
          return bool
          
      env <- Env.loadTestEnvironment
      result <- runRIO env runRioComposition -- :: Either Hex.HasMeshException Bool
      assertEqual "extract a Bool from a Either Hex.HasMeshException Bool" True result
   )
 runTestTT testExtractEitherExceptionRIO
