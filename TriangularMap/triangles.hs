module TriangularMap.Triangles where

import Data.List
import Data.Maybe
import Control.Monad

data BoundedTriangles = BoundedTriangles [Int] deriving (Show,Eq)
--foo :: Int -> Int -> [[[Int]]]
instance Ord BoundedTriangles where
  compare (BoundedTriangles (x:_)) (BoundedTriangles (y:_)) = compare x y
  compare _ _ = EQ

groupByLines :: [BoundedTriangles] -> [[BoundedTriangles]]
groupByLines =
  groupBy (\a b -> max' a < max' b) . sort where
    max' (BoundedTriangles xs) = maximum xs

tripoints :: Int -> Int -> [[BoundedTriangles]]
tripoints points_per_row rows =

  [ row (r * head_offset) ((r+1) * head_offset) |
    r <- [0..rows*2]] where

    head_offset = n + 1
    n = enumFromThen 0 2 !! points_per_row -- triangles count

    row r1 r2 =
      [ BoundedTriangles (point i r1 ++ point i r2) | 
        i <- enumFromThenTo 0 2 (n+2) ]

    point x r = [ x+i+r | 
          i   <- [-2..0],
          x+i >= 0, 
          x+i <= n ]
