--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import qualified VertexTest as VertexTest
import qualified FileWriterTest as FileWriterTest
import qualified ScriptingTest as ScriptingTest
import qualified LineTest as LineTest
import qualified ExceptionsTest as ExceptionsTest
import qualified CurveLoopTest as CurveLoopTest
import qualified PointTest as PointTest 
import qualified ListTest as ListTest
import qualified PolarTest as PolarTest
import qualified EnvironmentTest as EnvironmentTest
main = do
  FileWriterTest.runTests
  ScriptingTest.runTests
  LineTest.runTests
  ExceptionsTest.runTests
  CurveLoopTest.runTests
  ListTest.runTests
  VertexTest.runTests
  PointTest.runTests
  PolarTest.runTests
  EnvironmentTest.runTests
