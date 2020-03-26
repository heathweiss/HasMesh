--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import qualified VectorTest as Vector


main = do
  Vector.runTests
  
