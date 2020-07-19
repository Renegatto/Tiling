{-# LANGUAGE ScopedTypeVariables #-}
module ApproximateCalculations where
import Data.List
import Control.Arrow

data DiscreteFunction a b = NewDiscreteFunction [(a,b)] deriving (Show,Eq)
data Range a = Range a a deriving (Show,Eq)
data Density = Density Double deriving (Show,Eq)

bestApproximation :: forall a b. (Num a, Num b, Ord a)  =>  DiscreteFunction a b -> a -> (a,b)
bestApproximation (NewDiscreteFunction pairs) x =
    minimumBy (curry $ uncurry compare <<< difference *** difference) pairs where
        difference = abs . (x-) . fst

inversedFunction :: (Double -> b) -> Range Double -> Density -> DiscreteFunction b Double
inversedFunction f (Range from to) (Density density) =

    NewDiscreteFunction $ zip (map f xs) xs where

        xs = scanl (+) from $ take (round parts_count) $ repeat interval

        parts_count = density * size
        interval = size / parts_count -- dens = parts_count / range => parts_count = dens * range
        size = to - from -- +

totalyInversedInRange :: (Num b, Ord b) => (Double -> b) -> Range Double -> Density -> (b -> Double)
totalyInversedInRange f range density =
    snd . bestApproximation f' where
        f' = inversedFunction f range density