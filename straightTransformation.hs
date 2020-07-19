{-# LANGUAGE ScopedTypeVariables #-}

module StraightTransformation where
import qualified ReversedTransformation as Reversed

import qualified Point
import qualified Paraboloid
import Paraboloid(Paraboloid)
import Point (Point)

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

transformation :: FunctionReversing -> Paraboloid -> (Point -> Point)
transformation reversing (Paraboloid.Paraboloid focus) =  

    expand_on_points $ reversing $ Reversed.radius' focus where

        expand_on_points :: (Radius -> Radius) -> Point -> Point
        expand_on_points f p = Point.Cylindrical (f r, a, h) where
            (r,a,h) = Point.cylindrical p

hardcoded_configuration :: Point -> Point
hardcoded_configuration = 
    transformation (functionReversing 40.0 (CalcMethod.Density 20)) (Paraboloid.Paraboloid 15)