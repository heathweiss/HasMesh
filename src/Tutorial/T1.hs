{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |

-}
module Tutorial.T1() where

import RIO
import qualified System.IO as SIO
import qualified RIO.ByteString as B

import qualified Utils.FileWriter as FW
import qualified Utils.RunExceptions as HexR
import qualified Utils.Environment as Enviro
import qualified Utils.EnvironmentLoader as EnvLdr
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Point as Pnt
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.List as L
import qualified Gmsh.Point as Pnt
import qualified Gmsh.Line as Line


t1 :: IO ()
t1 = do
      let
        createDesign :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env, Enviro.HasDesignName env, Enviro.HasLineIdSupply env) => RIO env ()
        createDesign = do
          env <- ask
          geoFileHandleIORef <- view Enviro.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle $ ScrB.writeLC2

          let
            vertexs = [Geo.newVertex  0 0 0,    --{0, 0, 0, lc}
                       Geo.newVertex  0.1 0 0,  --{.1, 0,  0, lc};
                       Geo.newVertex 0.1 0.3 0, --{.1, .3, 0, lc};
                       Geo.newVertex 10 0.3 0    --{0,  .3, 0, lc};
                       ]
          
          _ <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" >>= Line.createLinesFromPoints
          return ()
      env <- EnvLdr.loadEnvironment
      designName <-  HexR.runEitherIO "designName" $ FW.newDesignName "t1"
      
      handle_ <- SIO.openFile (FW.designFilePath designName) WriteMode
      handleRef <- newIORef handle_
      runRIO (env {Enviro.env_geoFileHandle = handleRef}) createDesign
        `catch`
        (\(SomeException e) -> do
            handle' <- readIORef handleRef
            SIO.hClose handle'
            isOpen <- SIO.hIsOpen handle'
            runSimpleApp $ logInfo $ "handle is open: " <> displayShow isOpen
            runSimpleApp $ logInfo $ displayShow e
        )
      handle' <- readIORef handleRef
      SIO.hClose handle'
{-
t1 :: IO ()
t1 = do
      let
        createDesign :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env, Enviro.HasDesignName env) => RIO env ()
        createDesign = do
          env <- ask
          geoFileHandleIORef <- view Enviro.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle $ ScrB.writeLC1

          let
            vertexs = [Geo.newVertex  1 2 3, Geo.newVertex  4 5 6]
          points <- runRIO env $ Pnt.toPoints vertexs
         
          return ()
      env <- EnvLdr.loadEnvironment
      designName <-  HexR.runEitherIO "designName" $ FW.newDesignName "t1"
      
      handle_ <- SIO.openFile (FW.designFilePath designName) WriteMode
      handleRef <- newIORef handle_
      runRIO (env {Enviro.env_geoFileHandle = handleRef}) createDesign
        `catch`
        (\(SomeException e) -> do
            handle' <- readIORef handleRef
            SIO.hClose handle'
            isOpen <- SIO.hIsOpen handle'
            runSimpleApp $ logInfo $ "handle is open: " <> displayShow isOpen
            runSimpleApp $ logInfo $ displayShow e
        )
      handle' <- readIORef handleRef
      SIO.hClose handle'

-}
