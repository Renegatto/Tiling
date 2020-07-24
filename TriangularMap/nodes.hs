module TriangularMap.Nodes where
import TriangularMap.Triangles as Triangles
import Data.Triangle as Triangle

import qualified Data.Point as Point
import Data.Point (Point)

import Data.List

data Node a = Node a BoundedTriangles deriving (Eq,Show)
instance Functor Node where
    fmap f (Node x trgs) = Node (f x) trgs

type SideLength = Double
type Row = Int

line :: SideLength -> Row -> [BoundedTriangles] -> [Node Point]
line side_length row =
    zipWith Node (map point [0..])  where
        point i = Point.Cartesian (i * side_length, y_offset, 0)
        height = Triangle.heightFromSideLength side_length
        y_offset = height * fromIntegral row

calculate :: SideLength -> [BoundedTriangles] -> [[Node Point]]
calculate side = calculateGrouped side . Triangles.groupByLines

calculateGrouped :: SideLength -> [[BoundedTriangles]] -> [[Node Point]]
calculateGrouped side = zipWith (line side) [0..]