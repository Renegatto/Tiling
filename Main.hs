{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import Test.QuickCheck
import Data.List
import Data.Point as Point
import Data.Paraboloid as Paraboloid
import ReversedTransformation as RT
import qualified StraightTransformation
import qualified Tests
import qualified Tests.ReversedTransformation
import qualified Tests.SwitchingCoordinates

import Tiling

-- quickCheck $ withMaxSuccess 500 (transitive op3 op2)
-- quickCheck $ withMaxSuccess 50 (transformationSavingRadius op3)

run_tests = do
    _ <- Tests.ReversedTransformation.check_saving_distance
    _ <- Tests.ReversedTransformation.check_radius_growing
    Tests.SwitchingCoordinates.check_is_reversable

enumerate xs = zip (take (length xs) [0..]) xs
splitByGroups n xs = map (snd . unzip) $ groupBy f (enumerate xs) where
    f :: (Int,a) -> (Int,a) -> Bool --(\(i,_) -> (i `mod` 3) > 0)
    f (i0,_) (i1,_) = mod i0 n == 0 && mod i1 n /= 0
triple xs = (xs !! 0, xs !! 1, xs !! 2)
testSet :: String -> [(Double,Double,Double)]
testSet = map triple . filter ( (>=3). length) . splitByGroups 3 . map read . Prelude.lines 

uncurry3 f (a,b,c) = f a b c
   


compare_rt focus point = (Point.radius $ on_parab, Point.radius $ co_point) where
    on_parab = Paraboloid.onParaboloid parab point
    co_point = RT.reversedPointTransformation parab on_parab
    parab    = Paraboloid.Paraboloid focus

compare_d focus point = (distance_to_center on_parab, distance_to_center $ to_plane $ co_point) where
    on_parab = Paraboloid.onParaboloid parab point
    co_point = RT.reversedPointTransformation parab on_parab
    parab    = Paraboloid.Paraboloid focus
    to_plane p = (\(r,a,h) -> Point.Cylindrical (r,a,-focus)) $ Point.cylindrical p
    distance_to_center = Point.distance $ Point.Cartesian (0,0,-focus)

main = do
    numbers <- readFile "numbers.txt"
    print $ testSet numbers -- unlines $ map show $ map (compare_rt 3 . Cylindrical) (testSet numbers)s