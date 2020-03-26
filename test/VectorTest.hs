{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module VectorTest(runTests) where


import Test.HUnit
import qualified RIO.Text as T
import qualified Geometry.Vector as V


runTests = do
  
 let
  test1 = TestCase $ assertEqual
   "Vectors are equal"
   (True)
   ((V.Vertex 1 2 3) == (V.Vertex 1 2 3))
 runTestTT test1

 let
   test2 = TestCase $ assertEqual
     "Vectors are not equal"
     (False)
     ((V.Vertex 1 2 3) == (V.Vertex 11 2 3))
 runTestTT test2

 let
   test3 = TestCase $ assertEqual
     "Vectors are not equal"
     (False)
     ((V.Vertex 1 2 3) == (V.Vertex 1 22 3))
 runTestTT test3
 
 let
   test4 = TestCase $ assertEqual
     "Vectors are not equal"
      (False)
      ((V.Vertex 1 2 3) == (V.Vertex 1 2 33))
 runTestTT test4


