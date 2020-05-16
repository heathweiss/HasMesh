{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |

-}
module Tutorial.T1(rectangle2DFromVertex, rectangle2DFromFromPolarVertex, tenSidedShapeFromPolarVertex, rectangleWithSingleHole, rectangleWithSingleRaisedHole, put3holesInARectangle, ) where




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
import qualified Gmsh.CurveLoop as CL
import qualified Gmsh.PlaneSurface as PS
import qualified Geometry.Axis as Axis
import qualified Data.Bifunctor as Bif
import qualified List.Safe3 as L3
import List.Base((>>+))





-- | Create rectangular 2D shape from a ['Geometry.Vertex.Vertex']
rectangle2DFromVertex :: IO ()
rectangle2DFromVertex = 
  runVertexToShapeBldr
    [Geo.newVertex  0 0 0,    
     Geo.newVertex  0.1 0 0,  
     Geo.newVertex 0.1 0.3 0, 
     Geo.newVertex 0 0.3 0    
    ]
  



-- | Create the rectangular 2D shape using polar coordinates
rectangle2DFromFromPolarVertex :: IO ()
rectangle2DFromFromPolarVertex = do
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

-- | Create a 10 sided polygon using polar coordinates
tenSidedShapeFromPolarVertex :: IO ()
tenSidedShapeFromPolarVertex = do 
  let
    radius = 1
    vertexs =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [(  0, radius),
            ( 36, radius),
            ( 72, radius),
            (108, radius),
            (144, radius),
            (180, radius),
            (216, radius),
            (252, radius),
            (288, radius),
            (324, radius)
           ]
  runVertexToShapeBldr vertexs


-- | Create a 2D rectangle with a hole in the center
rectangleWithSingleHole :: IO ()
rectangleWithSingleHole = do
  let
    innerRadius = 1
    innerVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [( 60, innerRadius),
            (120, innerRadius),
            (240, innerRadius),
            (300, innerRadius)
           ]
    outerRadius = innerRadius + 1  
    outerVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [( 60, outerRadius),
            (120, outerRadius),
            (240, outerRadius),
            (300, outerRadius)
           ]
    createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
    createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC1
          safeVertexesInner <- HexR.runEitherRIO "safeVertexsInner" $ L3.toSafeList3 innerVertexes
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 outerVertexes 
          curveLoopInner <- runRIO env $ Pnt.toPoints safeVertexesInner >>= Line.toLines  >>= CL.toCurveLoop
          curveLoopOuter <- runRIO env $ Pnt.toPoints safeVertexesOuter >>= Line.toLines  >>= CL.toCurveLoop
          curveLoopsAdded <- HexR.runEitherRIO "curveLoopsAdded" (Right curveLoopOuter >>+ curveLoopInner) 
          _ <- runRIO env $ PS.toPlaneSurface curveLoopsAdded
          return ()
      
  env <- EnvLdr.loadEnvironment
  designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "t1"
  handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
  handleRef <- newIORef handle_
  runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign


-- | Create a 2D rectangle with a raised hole in the center.
-- Note: When meshed, the triangle radiate inwards on the xy plane, then sharply turn upwards to meet the inner hole.
-- The desired effect, as achieved in ChampCad, would to have triangles radiate in 3D to meet the inner shape, resulting in a smooth mesh,
-- Will need to manually join the inner and outer rectangles.
rectangleWithSingleRaisedHole :: IO ()
rectangleWithSingleRaisedHole = do
  let
    innerRadius = 1
    innerVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0.2)
           [( 60, innerRadius),
            (120, innerRadius),
            (240, innerRadius),
            (300, innerRadius)
           ]
    outerRadius = innerRadius + 1  
    outerVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [( 60, outerRadius),
            (120, outerRadius),
            (240, outerRadius),
            (300, outerRadius)
           ]
    createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
    createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC1
          safeVertexesInner <- HexR.runEitherRIO "safeVertexsInner" $ L3.toSafeList3 innerVertexes
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 outerVertexes 
          curveLoopInner <- runRIO env $ Pnt.toPoints safeVertexesInner >>= Line.toLines  >>= CL.toCurveLoop
          curveLoopOuter <- runRIO env $ Pnt.toPoints safeVertexesOuter >>= Line.toLines  >>= CL.toCurveLoop
          curveLoopsAdded <- HexR.runEitherRIO "curveLoopsAdded" (Right curveLoopOuter >>+ curveLoopInner) 
          _ <- runRIO env $ PS.toPlaneSurface curveLoopsAdded
          return ()
      
  env <- EnvLdr.loadEnvironment
  designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "t1"
  handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
  handleRef <- newIORef handle_
  runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign





-- | Create a rectangle with 3 holes in it.
put3holesInARectangle :: IO ()
put3holesInARectangle = do
  let
    innerRadius = 1
    topHoleVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 5)(Axis.ZAxis 0)
           [( 60, innerRadius),
            (120, innerRadius),
            (240, innerRadius),
            (300, innerRadius)
           ]
    bottomHoleVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis (-5))(Axis.ZAxis 0)
           [( 60, innerRadius),
            (120, innerRadius),
            (240, innerRadius),
            (300, innerRadius)
           ]
    centerHoleVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [(degree,innerRadius) | degree <- [0,10..350]]
     
    
    outerRadius = innerRadius + 10  
    outerVertexes =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [( 60, outerRadius),
            (120, outerRadius),
            (240, outerRadius),
            (300, outerRadius)
           ]
    createDesign :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => RIO env ()
    createDesign = do
          env <- ask
          geoFileHandleIORef <- view Env.geoFileHandleL
          geoFileHandle <- readIORef geoFileHandleIORef
          B.hPut geoFileHandle ScrB.writeLC
          safeVertexesTop <- HexR.runEitherRIO "safeVertexsTop" $ L3.toSafeList3 topHoleVertexes
          safeVertexesCenter <- HexR.runEitherRIO "safeVertexsCenter" $ L3.toSafeList3 centerHoleVertexes 
          safeVertexesBottom <- HexR.runEitherRIO "safeVertexsBottom" $ L3.toSafeList3 bottomHoleVertexes 
          safeVertexesOuter <- HexR.runEitherRIO "safeVertexsOuter" $ L3.toSafeList3 outerVertexes 
          curveLoopTop      <- runRIO env $ Pnt.toPoints safeVertexesTop    >>= Line.toLines  >>= CL.toCurveLoop
          curveLoopCenter   <- runRIO env $ Pnt.toPoints safeVertexesCenter >>= Line.toLines  >>= CL.toCurveLoop  
          curveLoopBottom   <- runRIO env $ Pnt.toPoints safeVertexesBottom >>= Line.toLines  >>= CL.toCurveLoop 
          curveLoopOuter    <- runRIO env $ Pnt.toPoints safeVertexesOuter  >>= Line.toLines  >>= CL.toCurveLoop
          curveLoopsAdded <- HexR.runEitherRIO "curveLoopsAdded" $ Right curveLoopOuter >>+ curveLoopTop >>+ curveLoopCenter >>+ curveLoopBottom
          _ <- runRIO env $ PS.toPlaneSurface curveLoopsAdded
          return ()
      
  env <- EnvLdr.loadEnvironment
  designName <-  HexR.runEitherIO "designName" $ Env.newDesignName "t1"
  handle_ <- SIO.openFile (Env.designFilePath designName) WriteMode
  handleRef <- newIORef handle_
  runRIO (env {Env.env_geoFileHandle = handleRef}) createDesign


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
          safeVertexs <- HexR.runEitherRIO "safeVertexs" $ L3.toSafeList3 vertexs 
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
        (\(Hex.NonUnique msg) -> do
            runSimpleApp $ logInfo $ "t1.hs err: " <> displayShow (Hex.SafeList3MinError msg)-- msg
            throwIO $ SomeException $ Hex.NonUnique msg
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
      
