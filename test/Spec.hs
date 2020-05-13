--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import qualified VertexTest as VertexTest
import qualified FileWriterTest as FileWriterTest
import qualified ScriptingTest as ScriptingTest
import qualified LineTest as LineTest
import qualified ExceptionsTest as ExceptionsTest
import qualified CurveLoopTest as CurveLoopTest
import qualified PointTest as PointTest 
import qualified List1Test as List1Test
import qualified PolarTest as PolarTest
import qualified EnvironmentTest as EnvironmentTest
import qualified PlaneSurfaceTest as PlaneSurfaceTest
import qualified List3Test as List3Test

main = do
  
  FileWriterTest.runTests
  
  ExceptionsTest.runTests
  EnvironmentTest.runTests
  VertexTest.runTests
  PointTest.runTests
  ScriptingTest.runTests
  PlaneSurfaceTest.runTests
  PolarTest.runTests
  List1Test.runTests
  List3Test.runTests
  LineTest.runTests
  CurveLoopTest.runTests
  
