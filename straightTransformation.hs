{-# LANGUAGE ScopedTypeVariables #-}

module StraightTransformation where
import qualified ReversedTransformation as Reversed

import qualified Data.Paraboloid as Paraboloid
import Data.Paraboloid (Paraboloid)
import qualified  Data.Point as Point
import Data.Point (Point)

import qualified CalculationMethods.ApproximateCalculations as CalcMethod
type Radius = Double
type Distance  = Double
type Density   = Int
type ReverseTransform = (Paraboloid -> Point -> Point)

data DiscreteStraightTransformation = DiscreteStraightTransformation [(Radius,Radius)]
-- DiscreteStraightTransformation : Radius -> Radius as in math definition (as cartesian product)
-- But in this case it's not total

uncurry3 f (a,b,c) = f a b c
curry3 f a b c = f (a,b,c)

type FunctionReversing = (Double -> Double) -> Double -> Double

functionReversing :: Distance -> CalcMethod.Density -> FunctionReversing
functionReversing dist dens fn = 
    as_function $ CalcMethod.totalyInversedInRange (CalcMethod.Range 0 dist) dens fn where
        as_function (CalcMethod.ApproximateFunction f) = f

transformation :: FunctionReversing -> Paraboloid -> Point -> Point
transformation reversing parab =  

    expand_on_points $ reversing $ Reversed.reversedRadiusTransformation parab where

        expand_on_points :: (Radius -> Radius) -> Point -> Point
        expand_on_points f p = 
            Paraboloid.onParaboloid parab $ Point.Cylindrical (f r, a, h) where
            (r,a,h) = Point.cylindrical p

hardcoded_configuration :: Point -> Point
hardcoded_configuration = 
    transformation (functionReversing 40.0 (CalcMethod.Density 20)) (Paraboloid.Paraboloid 15)