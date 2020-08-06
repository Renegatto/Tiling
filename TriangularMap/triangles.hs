module TriangularMap.Triangles 
  (
    tripoints,
    groupByLines,
    BoundedTriangles(BoundedTriangles)
  )
where

import Data.List
import Data.Maybe
import Control.Arrow
import Control.Applicative
import Control.Monad(join)
import Data.Function(on)

split_row first = 
  uncurry (++) 
  <<< pure *** (splitMultipleAt 3)
  <<< splitAt first

data BoundedTriangles = BoundedTriangles [Int] deriving (Show,Eq)
instance Ord BoundedTriangles where
  compare (BoundedTriangles (x:_)) (BoundedTriangles (y:_)) = compare x y
  compare _ _ = EQ

data Place = Even | Odd
data Columns = Columns Int deriving (Show,Eq)

trueOrNothing :: (a -> Bool) -> a -> Maybe a
trueOrNothing p x
  |p x       = Just x
  |otherwise = Nothing

row :: Place -> Int -> [[Int]]
row _ 0 = [[]]
row offset n =
  filter (not . null)
  $ map (
    filter (uncurry (&&) <<< (>= 0) &&& (<n))
    . flip map [0..2] 
    . (+))
  $ enumFromThenTo (-row_offset) (2-row_offset) n where

    row_offset = case offset of
      Even -> 2
      Odd  -> 1

pointsRow :: Columns -> Place -> Int -> [BoundedTriangles]
pointsRow (Columns n) place i =
  (zipWith bind `on` flip map (row place n) . map . (+) . (n *)) 
    i (i+1) 
  where
    bind a a' = BoundedTriangles $ a ++ a'

type PointsPerRow = Int
type Rows = Int

tripoints :: PointsPerRow -> Rows -> [[BoundedTriangles]]
tripoints points_per_row rows =
  every even (pointsRow (Columns n) Even) (pointsRow (Columns n) Odd)
  $ [0..rows] where
    n = points_per_row + 1
{-
  map (map $ BoundedTriangles. join)
  $ every even (split_row 1) (split_row 2)

  $ take rows
  $ map transpose
  $ iterate (map $ increase n) fisrt_pair where

    increase on = map $ map (on + 1 +)
    fisrt_pair = [increase (-n) xs, xs] where-- IS IT? We need overlaps
      xs = [0..n]
    

-}

every p f g =
  zipWith (\i -> if p i then f else g) [0..]

splitMultipleAt :: Int -> [a] -> [[a]]
splitMultipleAt n xs =
  takeWhile (not . null)
  $ map fst $ tail 
  $ iterate (splitAt n . snd) ([],xs)
    
groupByLines :: [BoundedTriangles] -> [[BoundedTriangles]]
groupByLines =
  groupBy (\a b -> max' a < max' b) . sort where
    max' (BoundedTriangles xs) = maximum xs

{-
import Data.List
import Data.Maybe
import Control.Monad
import Control.Arrow

data BoundedTriangles = BoundedTriangles [Int] deriving (Show,Eq)
--foo :: Int -> Int -> [[[Int]]]



{-
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
          x+i <= n ] -}

tripoints :: Int -> Int -> [[BoundedTriangles]]
tripoints points_per_row rows =

  map (map $ BoundedTriangles. join)
  $ every even (split_row 1) (split_row 2)

  $ take rows
  $ map transpose
  $ iterate (map increase) fisrt_pair where

    increase = map (n + 1 +)
    fisrt_pair = [[(-n-1)..(-1)],[0..n]]
    n = points_per_row + 1--(2 * points_per_row + 2) `div` 2

    split_row first = 
      uncurry (++) 
      <<< pure *** (splitMultipleAt 3)
      <<< splitAt first

every p f g =
  zipWith (\i -> if p i then f else g) [0..]

splitMultipleAt :: Int -> [a] -> [[a]]
splitMultipleAt n xs =
  takeWhile (not . null)
  $ map fst $ tail 
  $ iterate (splitAt n . snd) ([],xs)

-}