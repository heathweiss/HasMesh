{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
module Gmsh.LineZipTest(runTests) where
import RIO
import qualified RIO.Text as T
import qualified RIO.Map as Map
import qualified Data.Hashable as H
import qualified Prelude as P
import Test.HUnit
import qualified System.IO as SIO

import qualified Geometry.Vertex as V
import qualified Utils.Environment as Env
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Point as Pts  
import qualified Utils.EnvironmentLoader as EnvLdr 
import qualified Utils.Environment as Env
import qualified Gmsh.Line as Line
import qualified Gmsh.Point as Pnt
import qualified List.Safe1 as L1
import qualified List.Safe3 as L3
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import qualified Gmsh.LineZip as LZ
import qualified Geometry.Axis as Axis
import qualified Geometry.Polar as Polar

runTests = do
 P.putStrLn $ "=============== Line Zip Test ====================="

 {-
 All the test from LineZip have been moved to test/LineZipTest

 Next, need to create a module (Gmsh.Zip.Flex?) that allows the two lists to be zipped together in a flexible way. 
-}
 return ()
