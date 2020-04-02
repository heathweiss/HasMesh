{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module FileWriterTest(runTests) where

import RIO
import qualified RIO.Text as T
import Test.HUnit

runTests = do
-- ============================= Play with handles ==========================================
 let
  testHandles1 = TestCase 
   (do
      
      assertEqual "put a map into a IOVar" True False
   )
 runTestTT testHandles1
