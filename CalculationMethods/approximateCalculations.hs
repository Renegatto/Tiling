{-# LANGUAGE ScopedTypeVariables #-}
module CalculationMethods.ApproximateCalculations where
import Data.List
import Control.Arrow

data ApproximateFunction a b = ApproximateFunction (a -> b)
data DiscreteFunction a b = NewDiscreteFunction [(a,b)] deriving (Show,Eq)
data Range a = Range a a deriving (Show,Eq)
data Density = Density Double deriving (Show,Eq)

bestApproximation :: forall a b. (Num a, Num b, Ord a)  =>  DiscreteFunction a b -> a -> (a,b)
bestApproximation (NewDiscreteFunction pairs) x =
    minimumBy (curry $ uncurry compare <<< difference *** difference) pairs where
        difference = abs . (x-) . fst

inversedFunction :: Range Double -> Density -> (Double -> b) -> DiscreteFunction b Double
inversedFunction (Range from to) (Density density) f =

    NewDiscreteFunction $ zip (map f xs) xs where

        xs = scanl (+) from $ take (round parts_count) $ repeat interval

        parts_count = density * size
        interval = size / parts_count -- dens = parts_count / range => parts_count = dens * range
        size = to - from -- +

totalyInversedInRange :: (Num b, Ord b) => Range Double -> Density -> (Double -> b) ->ApproximateFunction b Double
totalyInversedInRange range density f =
    ApproximateFunction (snd . bestApproximation f') where
        f' = inversedFunction range density f

        
{-
bar :: (Double -> Double) -> ApproximateFunction Double Double
bar f = totalyInversedInRange (Range 3 18) (Density 0.5) f -}