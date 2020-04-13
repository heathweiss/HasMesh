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
import qualified Utils.Exceptions as Hex
import qualified Utils.Environment as Enviro
import qualified Utils.EnvironmentLoader as EnvLdr
import qualified Geometry.Geometry as Geo
import qualified Gmsh.Point as Pnt
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Utils.List as L
import qualified Gmsh.Point as Pnt
import qualified Gmsh.Line as Line
import qualified Gmsh.ID as ID


-- Base function that can run the various RIO fxs that does the geometry work
designLoader :: RIO Enviro.Environment () -> IO ()
designLoader createDesign = do
      
      env <- EnvLdr.loadEnvironment
      designName <-  HexR.runEitherIO "designName" $ FW.newDesignName "t1"
      
      handle_ <- SIO.openFile (FW.designFilePath designName) WriteMode
      handleRef <- newIORef handle_
      runRIO (env {Enviro.env_geoFileHandle = handleRef}) createDesign
        `catch`
        -- this is not catchint the SafeList3MinError
        (\(Hex.SafeList3MinError msg) -> do
            runSimpleApp $ logInfo $ "t1.hs err: " <> displayShow (Hex.SafeList3MinError msg)-- msg
            throwIO $ Hex.SafeList3MinError msg 
        )
        
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
      


{- |
['Geo.Vertex.Vertex'] -> 'Gmsh.PointIdList' -> 'Gmsh.LineIdList'

-}
t1a = do
  let
        createDesign :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env, Enviro.HasDesignName env, Enviro.HasLineIdSupply env) => RIO env ()
        createDesign = do
          env <- ask
          geoFileHandleIORef <- view Enviro.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle $ ScrB.writeLC2

          let
            vertexs = [Geo.newVertex  0 0 0,    
                       Geo.newVertex  0.1 0 0,  
                       Geo.newVertex 0.1 0.3 0, 
                       Geo.newVertex 0 0.3 0    
                      ]
          points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" 
          _ <- runRIO env $ Line.createLinesFromPoints points 
          
          return ()
  designLoader createDesign


{- |
['Geo.Vertex.Vertex'] ->  '[ID.Id ID.LineInt]'

t1b = do
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
          lines <- runRIO env $ Line.createLinesFromVertex "lines" vertexs 
          
          return ()
  designLoader createDesign
-}
{- |
['Geo.Vertex.Vertex'] -> 'Gmsh.PointIdList' from ['Geo.Vertex.Vertex'] -> 'Gmsh.[ID.Id ID.LineInt]'


t1c = do
  let
        createDesign :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env, Enviro.HasDesignName env, Enviro.HasLineIdSupply env) => RIO env ()
        createDesign = do
          env <- ask
          geoFileHandleIORef <- view Enviro.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle $ ScrB.writeLC2

          let
            vertexs = [Geo.newVertex  0 0 0,  
                       Geo.newVertex  0.1 0 0,
                       Geo.newVertex 0.1 0.3 0,
                       Geo.newVertex 0 0.3 0   
                      ]
          points <- runRIO env $ Pnt.toPoints vertexs >>= HexR.runEitherRIO "points" 
          _ <- runRIO env $ Line.createLinesFromPoints points 
          return ()
  designLoader createDesign
-}
{- |
['Geo.Vertex.Vertex'] ->  'LineIdList'
-}
t1d = do
  let
        createDesign :: (Enviro.HasPointIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env, Enviro.HasDesignName env, Enviro.HasLineIdSupply env) => RIO env ()
        createDesign = do
          env <- ask
          geoFileHandleIORef <- view Enviro.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle $ ScrB.writeLC2

          let
            vertexs = [Geo.newVertex  0 0 0,   
                       Geo.newVertex  0.1 0 0, 
                       Geo.newVertex 0.1 0.3 0,
                       Geo.newVertex 0 0.3 0  
                      ]
          lines <- runRIO env $ Line.createLinesFromVertex "lines" vertexs 
          
          return ()
  designLoader createDesign

