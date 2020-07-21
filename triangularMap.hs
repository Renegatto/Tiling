module TriangularMap where
--ghc 8.6.3
import Data.List
import Data.Maybe
import Control.Monad

type Line = [[Maybe Int]]
data Tripoint = 
  Tripoint (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) deriving (Show,Eq)

divisibleBy n x = mod x n == 0
unenumerate = snd . unzip
enumerate xs = 
  flip zip xs 
  $ take (length xs) [1..]

every n =
  unenumerate 
  . filter ({-not . -}divisibleBy n . fst) 
  . enumerate

overlaps with_length overlaping =
  every (with_length - overlaping) 
  . map (take with_length)
  . tails

positiveOrNothing x =
  if x > 0 then Just x else Nothing
  
-- tripoint t1 t1' t2 t2' t3 t3'
-- collectToTripoints :: [Line] -> [Tripoint]
collectToTripoints =
  map (map (tripoint . join) . transpose)
  . overlaps 2 1 where

    tripoint :: [Maybe Int] -> Tripoint
    tripoint triangles =
      Tripoint (get 1) (get 2) (get 3)
      (get 4) (get 5) (get 6) where
        get index = join $ lookup index xs
        xs = enumerate triangles    

do_with_rows :: Int -> Int -> [Line] -> [Line]
do_with_rows row_length columns rows =

  zipWith local_id_to_absolute initials rows where

    local_id_to_absolute :: Int -> Line -> Line
    local_id_to_absolute row_init = 
      map 
      $ map 
      $ fmap 
      $ (+) row_init

    initials :: [Int]
    initials = map (* row_length) [0..columns - 1]

rows :: Int -> Int -> [Line] 
rows row_length columns =
  take columns
  $ repeat
  $ filter (any isJust)
  $ overlaps 3 1
  $ map positiveOrNothing
  $ [-1..row_length] ++ [-2,-1] where


--main = print $ collectToTripoints $ do_with_rows 5 4 $ rows 5 7