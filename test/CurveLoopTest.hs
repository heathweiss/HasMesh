{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CurveLoopTest(runTests) where

import RIO
import Test.HUnit
import qualified Prelude as P

runTests = do
 P.putStrLn $ "=============== Cureve Loop Test ====================="  
 -- ===================================  ===================
 let
  getSomeTestsHappening = TestCase $ assertEqual
   "What, no tests"
   (True)
   (False) 
 runTestTT getSomeTestsHappening
