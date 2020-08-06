{-# LANGUAGE ScopedTypeVariables #-}

module TriangularMap.Nodes where
import TriangularMap.Triangles as Triangles
import Data.Triangle as Triangle

import qualified Data.Point as Point
import Data.Point (Point)

import Data.List

data Node a = Node a BoundedTriangles deriving (Show)
instance Functor Node where
    fmap f (Node x trgs) = Node (f x) trgs
instance (Eq a) => Eq (Node a) where
    (Node x xs) == (Node y ys) = x == y && xs == ys
instance (Eq a) => Ord (Node a) where
    compare (Node _ xs) (Node _ ys) = compare xs ys

type SideLength = Double
type Row = Int

hasTriangle :: Int -> Node a -> Bool
hasTriangle with_number (Node _ (BoundedTriangles trgs)) =
    any (== with_number) trgs 

line :: SideLength -> Row -> [BoundedTriangles] -> [Node Point]
line side_length row =
    zipWith Node (map point [0..])  where
        point i = Point.Cartesian (i * side_length, y_offset, 0)
        height = Triangle.heightFromSideLength side_length
        y_offset = height * fromIntegral row

calculate :: SideLength -> [BoundedTriangles] -> [[Node Point]]
calculate side = calculateGrouped side . Triangles.groupByLines

shiftLineByX :: Double -> [Node Point] -> [Node Point]
shiftLineByX offset = map (fmap shift) where
     shift p = Point.Cartesian (x + offset,y,z) where
        (x,y,z) = Point.cartesian p

calculateGrouped :: SideLength -> [[BoundedTriangles]] -> [[Node Point]]
calculateGrouped side = 
    zipWith shift_if_even [1..]
    . zipWith (line side) [0..] where
        shift_if_even i line 
            |i `mod` 2 == 0 = 
                shiftLineByX (side / 2) line
            |otherwise      = line

groupByLines :: (Eq a) => [Node a] -> [[Node a]]
groupByLines =
  groupBy (\a b -> max' a < max' b) . sort where
    max' (Node _ (BoundedTriangles xs)) = maximum xs

data Nodes a = Nodes [Node a] deriving (Eq,Show)
instance Foldable Nodes where
    foldr f acc (Nodes (x:xs)) = 
        foldr f (acc' x) $ Nodes xs where
            acc' (Node y _) = f y acc
    foldr f acc (Nodes []) = acc
instance Functor Nodes where
    fmap f (Nodes xs) = Nodes $ map (fmap f) xs
{-
 seqNode :: Applicative f => Node (f a) -> f (Node a)
 seqNode (Node fx trgs) = fmap (flip Node trgs) fx where

bar :: Applicative f => Nodes (f a) -> [f (Node a)]
bar (Nodes fxs) =  map seqNode fxs

baz :: Applicative f => [f (Node a)] -> f (Nodes a)
baz = fmap Nodes . sequenceA

goo :: Applicative f => Nodes (f a) -> f (Nodes a)
goo = fmap Nodes . sequenceA . bar-}

instance Traversable Nodes where
    sequenceA (Nodes fxs) = 
        fmap Nodes . sequenceA . map seqNode $ fxs where
        seqNode (Node fx trgs) = fmap (flip Node trgs) fx    

data Lines a = Lines [[Node a]]
instance Functor Lines where
    fmap f (Lines xs) = Lines $ map (map $ fmap f) xs

subArea :: Int -> Int -> Lines a -> Lines a
subArea max_lines max_points (Lines xs) =
    Lines . map (limit max_points) . limit max_lines $ xs where
        limit n = flip (zipWith const) [0..n]
        
--be careful, maybe you dont need this:
scaled :: Double -> Lines Point -> Lines Point
scaled by = fmap (\p -> Point.map_z f $ Point.map_y f $ Point.map_x f p) where
    f x = x * by

triangle :: Int -> Nodes a -> [a]
triangle triangle_number (Nodes nodes) =
    take 3 . map value_of . filter (hasTriangle triangle_number) $ nodes where
        value_of (Node x _) = x

countTriangles :: Eq a => Nodes a -> Int
countTriangles (Nodes nodes) = (+1) $ last_triangle $ last $ sort nodes where
    last_triangle (Node _ (BoundedTriangles xs)) = last $ sort xs

nodesToTriangles :: Eq a => Nodes a -> [[a]]
nodesToTriangles nodes=
    flip map [0..(countTriangles nodes) -1] $ flip triangle nodes

maxDistancesBetweenApexes :: Nodes Point -> [Double]
maxDistancesBetweenApexes = 
    map maximum
    . filter (not . null)
    . map distances
    . nodesToTriangles where
        distances :: [Point] -> [Double]
        distances = 
            map (\xs -> Point.distance (head xs) (head $ tail xs)) 
            . filter ((2 <=) . length) . nub . map (take 2) . permutations 