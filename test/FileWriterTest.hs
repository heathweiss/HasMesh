{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module FileWriterTest(runTests) where

import RIO
import qualified RIO.Text as T
import Test.HUnit

runTests = do
 putStrLn $ "=============== FileWriter Tests ====================="  
-- ============================= Play with handles ==========================================
 let
  testHandles1 = TestCase 
   (do
      
      assertEqual "there are no tests yet" True False
   )
 runTestTT testHandles1
