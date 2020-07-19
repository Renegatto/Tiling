module Tests.ReversedTransformation where

import qualified Test.QuickCheck as Check
import Tests hiding (mistake3)

import qualified Paraboloid
import Paraboloid (Paraboloid)
import qualified Point
import Point (Point)

import ReversedTransformation

type Transformation = (Point -> Point)
type Coords = (Double,Double,Double)

savingValue :: (Point -> Double) -> Mistake -> Transformation -> Point -> Bool
savingValue value mist f p =
  mistake mist (value $ f p) (value p)

savingAngle  = savingValue Point.angle
savingRadius = savingValue Point.radius

prop_savingRadius :: Double -> Double -> Coords -> Bool
prop_savingRadius _ 0 _ = True
prop_savingRadius mist focus p =

  savingRadius (Mistake mist) (reversedTransformation parab) point where

    point = Paraboloid.onParaboloid parab (Point.Cartesian p)
    parab = Paraboloid.Paraboloid focus

check_saving_radius = Check.quickCheck $ Check.withMaxSuccess 100 prop where
    prop = (prop_savingRadius 5)