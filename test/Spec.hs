--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import qualified VertexTest as VertexTest
import qualified FileWriterTest as FileWriterTest
import ScriptingTest as ScriptingTest

main = do
  VertexTest.runTests
  FileWriterTest.runTests
  ScriptingTest.runTests
