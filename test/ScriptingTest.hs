{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ScriptingTest(runTests) where

import RIO
import Test.HUnit
import qualified Scripting.Scripting as Script
import qualified RIO.Text as T
import qualified Geometry.Geometry as Geo

runTests = do
 putStrLn $ "=============== Scripting Tests ====================="
-- ============================= Separator and comments ==========================================
 let
  testSeparator = TestCase $ assertEqual
   "output a separator"
   ("///////////////////////////////////////////////////////////////")
   (Script.separator)
 runTestTT testSeparator

 let
  testComment = TestCase $ assertEqual
   "output a comment"
   ("\n//my comment")
   (Script.comment "my comment")
 runTestTT testComment

-- ============================= Vertex tests ==========================================

 let
  testPoint1 = TestCase $ assertEqual
   "output a point"
   ("Point(1) = {1.0,2.0,3.0,lc};")
   (Script.point (Geo.newVertex 1 2 3) (Geo.PointId 1))
 runTestTT testPoint1


-- ============================= lc tests ==========================================
 let
  testLc1 = TestCase $ assertEqual
   "output an lc1"
   ("lc = 1e-1;")
   (Script.lc (Script.newLC1))
 runTestTT testLc1

 let
  testLc2 = TestCase $ assertEqual
   "output an lc2"
   ("lc = 1e-2;")
   (Script.lc (Script.newLC2))
 runTestTT testLc2

 let
  testLc3 = TestCase $ assertEqual
   "output an lc3"
   ("lc = 1e-3;")
   (Script.lc (Script.newLC3))
 runTestTT testLc3
