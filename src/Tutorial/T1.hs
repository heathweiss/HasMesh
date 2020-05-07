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
import qualified Gmsh.PlaneSurface as PS
import qualified Geometry.Axis as Axis
import qualified Data.Bifunctor as Bif





-- | Create the .geo shape from a ['Geometry.Vertex.Vertex']
fromVertex :: IO ()
fromVertex = 
  runVertexToShapeBldr
    [Geo.newVertex  0 0 0,    
     Geo.newVertex  0.1 0 0,  
     Geo.newVertex 0.1 0.3 0, 
     Geo.newVertex 0 0.3 0    
    ]
  



-- | Create the .geo shape from a ['Geometry.Vertex.Vertex'] which are generated using 'Polar.newVertexFromPolarCoordinatesTuples'
-- Had to reduce it in size, as meshing is causing 50x more nodes to be created than 'fromVertex' when they are same size.
-- At originl radius of 50, had to kill gmsh binary
fromPolarVertex :: IO ()
fromPolarVertex = do
  let
    radius = 1
    vertexs =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [( 60, radius),
            (120, radius),
            (240, radius),
            (300, radius)
           ]
  runVertexToShapeBldr vertexs

{-
fromPolarVertex :: IO ()
fromPolarVertex = do
  let
    radius = 1
    vertexs =
      Polar.newVertexes (Polar.Origin (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0) )
        $ map (Bif.bimap Polar.Degree  Polar.Radius )
           [( 60, radius),
            (120, radius),
            (240, radius),
            (300, radius)
           ]
  runVertexToShapeBldr vertexs

-}


-- Generates a gmsh shape and file from an input of ['Geometry.Vertex.Vertex']
runVertexToShapeBldr :: [Geo.Vertex] -> IO ()
runVertexToShapeBldr vertexs = do
  let
        createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
        
        createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC1
          safeVertexs <- HexR.runEitherRIO "safeVertexs" $ L.toSafeList3 vertexs 
          _ <- runRIO env $ Pnt.toPoints safeVertexs >>= Line.toLines  >>= CL.toCurveLoop  >>= PS.toPlaneSurface
          return ()
          
  designLoader createDesign
  
-- Takes care of the environment and handle. 
designLoader :: RIO Env.Environment () -> IO ()
designLoader createDesign = do
      env <- EnvLdr.loadEnvironment
      designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "t1"
      handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
      handleRef <- newIORef handle_
      runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign
      
        `catch`
        (\(Hex.SafeList3MinError msg) -> do
            runSimpleApp $ logInfo $ "t1.hs err: " <> displayShow (Hex.SafeList3MinError msg)-- msg
            throwIO $ SomeException $ Hex.SafeList3MinError msg
        )
        `catch`
        (\(Hex.NonUniqueVertex msg) -> do
            runSimpleApp $ logInfo $ "t1.hs err: " <> displayShow (Hex.SafeList3MinError msg)-- msg
            throwIO $ SomeException $ Hex.NonUniqueVertex msg
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
      
