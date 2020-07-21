module TriangularMap.Triangles where

import Data.List
import Data.Maybe
import Control.Monad

data BoundedTriangles = 
  BoundedTriangles (Maybe Int) (Maybe Int) (Maybe Int) 
    (Maybe Int) (Maybe Int) (Maybe Int) deriving (Show,Eq)
type Line = [[Maybe Int]]

collectedTriangles :: [Line] -> [[BoundedTriangles]]
collectedTriangles =
  map (map (wrap . join) . transpose)
  . overlaps 2 1 where

    wrap :: [Maybe Int] -> BoundedTriangles
    wrap triangles =
      BoundedTriangles (get 1) (get 2) (get 3)
      (get 4) (get 5) (get 6) where
        get index = join $ lookup index xs
        xs = enumerate triangles    

globally_enumerated :: Int -> Int -> [Line] -> [Line]
globally_enumerated row_length columns rows =

  zipWith local_id_to_absolute initials rows where

    local_id_to_absolute :: Int -> Line -> Line
    local_id_to_absolute = map . map . fmap . (+)

    initials :: [Int]
    initials = map (row_length *) [0..columns - 1]

rows :: Int -> Int -> [Line] 
rows row_length columns =
  take columns
  $ repeat
  $ filter (any isJust)
  $ overlaps 3 1
  $ map maybePositive
  $ [-1..row_length] ++ [-2,-1]

every nth =
  unenumerate 
  . filter (divisibleBy nth . fst) 
  . enumerate

overlaps with_length overlaping =
  every (with_length - overlaping) 
  . map (take with_length)
  . tails

divisibleBy n x = mod x n == 0

unenumerate :: [(Int,a)] -> [a]
unenumerate = snd . unzip

enumerate :: [a] -> [(Int,a)]
enumerate = zip [1..]

maybePositive :: (Num n, Ord n) => n -> Maybe n
maybePositive x =
  if x > 0 then Just x else Nothing


--main = print $ collectTriangles $ globally_enumerated 5 4 $ rows 5 7