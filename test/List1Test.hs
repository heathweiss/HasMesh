{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GADTs #-}
module List1Test(runTests) where

import RIO
import Test.HUnit
import qualified Prelude as P
import qualified Utils.Environment as Env
import qualified List.Safe1 as L1
import qualified List.Safe3 as L3
import qualified Utils.Exceptions as Hex
import qualified Utils.RunExceptions as HexR
import qualified Geometry.Vertex as V
import List.Base ((>>+))

runTests = do
 P.putStrLn $ "=============== List1 Test ====================="  


 

 

