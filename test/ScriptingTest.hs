{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ScriptingTest(runTests) where

import RIO
import Test.HUnit
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Gmsh.ToScript.Common as SCom
import qualified RIO.Text as T
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Gmsh as Gmsh

runTests = do
 putStrLn $ "=============== Scripting Tests ====================="
-- ============================= Separator and comments ==========================================
 let
  testSeparator = TestCase $ assertEqual
   "output a separator"
   ("///////////////////////////////////////////////////////////////")
   (SCom.writeSeparator)
 runTestTT testSeparator

 let
  testComment = TestCase $ assertEqual
   "output a comment"
   ("\n//my comment")
   (SCom.writeComment "my comment")
 _ <- runTestTT testComment
 
-- ============================= Vertex tests ==========================================

 let
  testPoint1 = TestCase $ assertEqual
   "output a point"
   "\nPoint(1) = {1.0,2.0,3.0,lc};"
   (ScrB.writePoint (Geo.newVertex 1 2 3) Gmsh.initialId)
 _ <- runTestTT testPoint1

-- ============================= lc tests ==========================================
 let
  testLc1 = TestCase $ assertEqual
   "output an 1c1"
   "lc = 1e-1;"
   ScrB.writeLC1
 _ <- runTestTT testLc1

 let
  testLc2 = TestCase $ assertEqual
   "output an 2c2"
   "lc = 1e-2;"
   ScrB.writeLC2
 _ <- runTestTT testLc2


 let
  testLc3 = TestCase $ assertEqual
   "output an 1e-3"
   "lc = 1e-3;"
   ScrB.writeLC3
 _ <- runTestTT testLc3
 


-- ============================ LINE tests =============================================

 let
  testLine = TestCase $ assertEqual
   "write a line"
   "\nLine(1) = {1,2};"
   (ScrB.writeLine (Gmsh.initialId :: Gmsh.Id Gmsh.LineInt) (Gmsh.initialId::Gmsh.Id Gmsh.PointInt) (Gmsh.incr Gmsh.initialId::Gmsh.Id Gmsh.PointInt))
 runTestTT testLine
 

