{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |

-}
module Tutorial.T1(t1_linesFromPoints, t1_linesFromPolarTuples) where

import RIO
import qualified System.IO as SIO
import qualified RIO.ByteString as B
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import qualified Utils.Environment as Env
import qualified Geometry.Geometry as Geo
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Gmsh.Point as Pnt
import qualified Gmsh.Line as Line
import qualified Geometry.Vertex as V
import qualified Geometry.Polar as Polar



-- Base function that can run the various RIO fxs that does the geometry work
designLoader :: RIO Env.Environment () -> IO ()
designLoader createDesign = do
      
      env <- Env.loadEnvironment
      designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "t1"
      
      handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
      handleRef <- newIORef handle_
      runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign
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
Create the vertex, then the points, then the lines
-}
t1_linesFromPoints = do
  let
        createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env) => RIO env ()
        createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC2
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



-- | Create the lines directly from the vertex.
t1_linesFromVertex = do
  let
        createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env) => RIO env ()
        
        createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC2

          let
            vertexs = [Geo.newVertex  0 0 0,   
                       Geo.newVertex  0.1 0 0, 
                       Geo.newVertex 0.1 0.3 0,
                       Geo.newVertex 0 0.3 0  
                      ]
          _ <- runRIO env $ Line.createLinesFromVertex "lines" vertexs
          return ()
  designLoader createDesign

-- | Generate the vertex using polar coordinates. Then create the lines directly from the vertex.
t1_linesFromPolarTuples = do
  let
    createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env) => RIO env ()
    createDesign = do
        env <- ask
        geoFileHandleIORef <- view Env.geoFileHandleL
        geoFileHandle <- readIORef geoFileHandleIORef
        --writing the lc2 before the let statement causes it to be eval'd.
        a <- B.hPut geoFileHandle ScrB.writeLC2
        --evaluate a :maybe this would force it to evaluate
        let
          radius = 50
          height = 0
          vertexs =
            Polar.newVertexFromPolarCoordinatesTuples
              [(60, radius, height),
               (120, radius, height),
               (240, radius, height),
               (300, radius, height)
              ]
        _ <- runRIO env $ Line.createLinesFromVertex "lines" vertexs
        return ()
        
  designLoader createDesign

{-
t1d = do
  let
        createDesign :: (Enviro.HasIdSupply env, Enviro.HasPointIdMap env, Enviro.HasGeoFileHandle env, Enviro.HasDesignName env) => RIO env ()
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

-}
