--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import qualified VertexTest as VertexTest
import qualified FileWriterTest as FileWriterTest
import qualified ScriptingTest as ScriptingTest
import qualified LineTest as LineTest
import qualified ExceptionsTest as ExceptionsTest

main = do
  VertexTest.runTests
  FileWriterTest.runTests
  ScriptingTest.runTests
  LineTest.runTests
  ExceptionsTest.runTests
