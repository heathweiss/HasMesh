module Gmsh.Status(Closed(), Open()) where

-- | A state where a list of items wraps around on itself and so the 1st item == last item.
--
-- Eg: 'Gmsh.Line.LineIdSafe3List' where the fst 'Gmsh.ID.Id Gmsh.ID.PointId' of the first line  == last 'Gmsh.ID.Id Gmsh.ID.PointId' of the last line, resulting in lines that wrap around and form a closed polygon.
data Closed = Closed


-- | A state where a list of items does not wrap around on itself, and so the 1st item /= last item.
--
-- Eg: 'Gmsh.Point.PointIdList' where the fst 'Gmsh.ID.Id Gmsh.ID.PointId' /= last 'Gmsh.ID.Id Gmsh.ID.PointId' resulting in ids that do wrap around and form a closed polygon.
data Open = Open
