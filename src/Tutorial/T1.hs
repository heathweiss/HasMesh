{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |

-}
module Tutorial.T1(fromVertex, fromPolarVertex) where


import RIO
import qualified System.IO as SIO
import qualified RIO.ByteString as B
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import qualified Utils.Environment as Env
import qualified Utils.EnvironmentLoader as EnvLdr 
import qualified Geometry.Geometry as Geo
import qualified Gmsh.ToScript.BuiltIn as ScrB
import qualified Gmsh.Point as Pnt
import qualified Gmsh.Line as Line
import qualified Geometry.Vertex as V
import qualified Geometry.Polar as Polar
import qualified Utils.List as L
import qualified Gmsh.CurveLoop as CL

-- Base function that can run the various RIO fxs that does the geometry work
designLoader :: RIO Env.Environment () -> IO ()
designLoader createDesign = do
      
      env <- EnvLdr.loadEnvironment
      
      designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "t1"
      
      handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
      handleRef <- newIORef handle_
      runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign
        `catch`
        
        (\(Hex.NonUniqueVertex msg) -> do
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
Create the .geo shape from a ['Geometry.Vertex.Vertex']
-}
fromVertex :: IO ()
fromVertex = do
  let
        createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
        
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
          safeVertexs <- HexR.runEitherRIO "safeVertexs" $ L.toSafeList3 vertexs
          points <- runRIO env $ Pnt.toPoints safeVertexs
          
          lines <- runRIO env $ Line.toLines points
          _ <- runRIO env $ CL.toCurveLoop lines
          return ()
  designLoader createDesign
{-
fromVertex :: IO ()
fromVertex = do
  let
        createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
        
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
          safeVertexs <- HexR.runEitherRIO "safeVertexs" $ L.toSafeList3 vertexs
          points <- runRIO env $ Pnt.toPoints safeVertexs
          
          _ <- runRIO env $ Line.toLines points 
          
          return ()
  designLoader createDesign

-}

-- | Create the .geo shape using ' Polar.newVertexFromPolarCoordinatesTuples'
fromPolarVertex :: IO ()
fromPolarVertex = do
  let
    createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
    
    createDesign = do
        env <- ask
        geoFileHandleIORef <- view Env.geoFileHandleL
        geoFileHandle <- readIORef geoFileHandleIORef
        a <- B.hPut geoFileHandle ScrB.writeLC2
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
        safeVertexs <- HexR.runEitherRIO "safeVertexs" $ L.toSafeList3 vertexs
        _ <- runRIO env $ Pnt.toPoints safeVertexs >>= Line.toLines
        return ()
  designLoader createDesign


