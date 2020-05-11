{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |

-}
module Tutorial.T1(fromVertex, fromPolarVertex, put1holeInIt, put3holesInARectangle, fromPolarVertex10Sided) where


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
import qualified List.Safe3 as L3
import Utils.Add((+++))





-- | Create the .geo shape from a ['Geometry.Vertex.Vertex']
fromVertex :: IO ()
fromVertex = 
  runVertexToShapeBldr
    [Geo.newVertex  0 0 0,    
     Geo.newVertex  0.1 0 0,  
     Geo.newVertex 0.1 0.3 0, 
     Geo.newVertex 0 0.3 0    
    ]
  



-- | Create the rectangle using polar coordinates
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

-- | Create a 10 sided polygon using polar coordinates
fromPolarVertex10Sided :: IO ()
fromPolarVertex10Sided = do 
  let
    radius = 1
    vertexs =
      Polar.newVertexes (Axis.XAxis 0) (Axis.YAxis 0)(Axis.ZAxis 0)
           [( 36, radius),
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


-- | Create a polygon with a hole in the center
put1holeInIt :: IO ()
put1holeInIt = do
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
          curveLoopInner <- runRIO env $ Pnt.toPoints safeVertexesInner >>= Line.toLines  >>= CL.toCurveLoop  --  >>= PS.toPlaneSurface
          curveLoopOuter <- runRIO env $ Pnt.toPoints safeVertexesOuter >>= Line.toLines  >>= CL.toCurveLoop  --  >>= PS.toPlaneSurface
          _ <- runRIO env $ PS.toPlaneSurface $ curveLoopOuter +++ curveLoopInner
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
           [( 20, innerRadius),
            ( 40, innerRadius),
            ( 30, innerRadius),
            ( 60, innerRadius),
            (120, innerRadius),
            (240, innerRadius)
            --(300, innerRadius)
           ]
                        --[(degree,innerRadius) | degree <- [0,10,20,30,40,50,60]]
     {-      [(  0, innerRadius),
            ( 10, innerRadius),
            ( 20, innerRadius),
            ( 30, innerRadius),
            ( 40, innerRadius),
            ( 50, innerRadius),
            ( 60, innerRadius),
            ( 70, innerRadius),
            ( 80, innerRadius),
            ( 90, innerRadius),
            ( 1000, innerRadius),
            (120, innerRadius),
            (240, innerRadius),
            (300, innerRadius)
      
  ]-}
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
          _ <- runRIO env $ PS.toPlaneSurface $ curveLoopOuter +++ curveLoopTop +++ curveLoopCenter +++ curveLoopBottom 
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
      
