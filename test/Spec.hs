--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import qualified VertexTest as VertexTest
import qualified FileWriterTest as FileWriterTest
import qualified ScriptingTest as ScriptingTest
import qualified LineTest as LineTest

main = do
  VertexTest.runTests
  FileWriterTest.runTests
  ScriptingTest.runTests
  LineTest.runTests
