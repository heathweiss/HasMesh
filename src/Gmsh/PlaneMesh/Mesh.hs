{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{- |
Zip together vertexes, and the subsequent gmsh points,lines,curve loops, and planes, in a mesh manner.

Examples generating .geo files are in 

import qualified Gmsh.PlaneMesh.Mesh as Mesh
-}
module Gmsh.PlaneMesh.Mesh(meshVertex, VertexMeshResults(..), meshVertexToPoints) where
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
--import qualified Gmsh.Point as Pts  
import qualified Utils.EnvironmentLoader as EnvLdr 
import qualified Utils.Environment as Env
import qualified Gmsh.Line as Line
import qualified Gmsh.Point as Pnt
import qualified Gmsh.PlaneSurface as PS
import qualified List.Safe1 as L1
import qualified List.Safe3 as L3
import qualified Utils.RunExceptions as HexR
import qualified Utils.Exceptions as Hex
import qualified Geometry.Axis as Axis
import qualified Geometry.Polar as Polar
import qualified Gmsh.Line as Line
import qualified Gmsh.CurveLoop as CL
import qualified Utils.Environment as Env
import qualified Gmsh.PlaneMesh.LineZip as LZ




data VertexMeshResults =  DoubleVertexMesh {_zipInner :: [V.Vertex],
                                           _zipOuter :: [V.Vertex],
                                           _zipResults :: [[V.Vertex]]}
  deriving (Eq,Show)



-- | Combine the inner and outer ['V.Vertex'] by taking a single vertex from each [V.Vertex].
-- | If the ['V.Vertex'] are not even lengths, finish of combinations using last point of shortest [].
meshVertex :: VertexMeshResults -> VertexMeshResults
meshVertex  (DoubleVertexMesh [] [] working) = DoubleVertexMesh [] [] working
meshVertex  (DoubleVertexMesh [i] [o] working) = DoubleVertexMesh [i] [o] working
meshVertex  (DoubleVertexMesh (iInitial:inner) (oInitial:outer) working) = 
  meshVertexR inner iInitial outer oInitial working
  where
  --add in the last leading points used params, and the working list of [V.Vertex]
  meshVertexR :: [V.Vertex] -> V.Vertex -> [V.Vertex] -> V.Vertex -> [[V.Vertex]] -> VertexMeshResults
  meshVertexR [] prevI [] prevO working =
    DoubleVertexMesh [] [] $ reverse ([prevI,prevO,oInitial,iInitial]:working) --close the shape by joining the last vertexes to the intial vertexes.
  meshVertexR [] prevI (o:outer) prevO working =
    meshVertexR [] prevI outer o $ [prevI,prevO,o] : working
  meshVertexR (i:inner) prevI [] prevO working =
    meshVertexR inner i [] prevO $ [prevI,prevO,i] : working 
  meshVertexR (i:inner) prevI (o:outer) prevO working =
    meshVertexR inner i outer o $ [prevI,prevO,o,i] : working


-- | Now take the resulting [[V.Vertex]] workingList, from the VertexMeshResults returned by 'meshVertex', turn them into
-- [L3.PointIdSafe3List] so that they can be processed into a [[Gmsh.Point.PointIdSafe3List]].
-- These can then be turned into planes using the Gmsh.PlaneMesh.LineZip module.
meshVertexToPoints :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => VertexMeshResults -> RIO env [L3.PointIdSafe3List]
meshVertexToPoints (DoubleVertexMesh inner outer (w:working)) = do
  env <- ask
   
  let
    -- Add the workingSafeList of [L3.PointIdSafe3List], which is the result of meshVertexToPoints.
    meshVertexToPointsRecur :: (Env.HasIdSupply env, Env.HasPointIdMap env, Env.HasGeoFileHandle env, Env.HasScriptWriter env) => VertexMeshResults -> [L3.PointIdSafe3List] -> RIO env [L3.PointIdSafe3List]
    meshVertexToPointsRecur (DoubleVertexMesh _ _ []) workingSafeList = return workingSafeList
    meshVertexToPointsRecur (DoubleVertexMesh innerRecur outerRecur (w:working)) workingSafeList = do
      env <- ask
      let
        safeVertex = HexR.runEither "jkl" $  L3.toSafeList3 w 
      temp <- runRIO env ( Pnt.toPoints safeVertex) 
      runRIO env (meshVertexToPointsRecur (DoubleVertexMesh innerRecur outerRecur (working)) (temp : workingSafeList))
  runRIO env $ meshVertexToPointsRecur (DoubleVertexMesh inner outer (w:working)) []
  


{-
-}
