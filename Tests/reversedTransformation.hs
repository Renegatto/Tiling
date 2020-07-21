module Tests.ReversedTransformation where

import qualified Test.QuickCheck as Check
import Tests hiding (mistake3)

import qualified Data.Paraboloid as Paraboloid
import Data.Paraboloid (Paraboloid)
import qualified  Data.Point as Point
import Data.Point (Point)

import ReversedTransformation

type Transformation = (Point -> Point)
type Coords = (Double,Double,Double)

savingValue :: (Point -> Double) -> Mistake -> Transformation -> Point -> Bool
savingValue value mist f p =
  mistake mist (value $ f p) (value p)

savingAngle  = savingValue Point.angle
savingRadius = savingValue Point.radius
savingDistanceToParaboloidCenter (Paraboloid.Paraboloid focus) = 
  savingValue $ Point.distance $ Point.Cartesian (0,0,(-focus))


ifValidInput focus r bool = if focus > 0 && r >= 0 then bool else True

--it is not saving radius, but radius to parab's center, which is having (z = -focus)
-- so need to compare distance to (0,0,-focus) before and after transformation

prop_savingDistance :: Double -> Double -> Coords -> Bool
prop_savingDistance _ 0 _ = True
prop_savingDistance mist focus p =

  ifValidInput focus (fst3 p)
  $ savingDistanceToParaboloidCenter 
    parab (Mistake mist) (to_plane . reversedPointTransformation parab) point where

    point = Paraboloid.onParaboloid parab (Point.Cartesian p)
    parab = Paraboloid.Paraboloid focus
    to_plane p = (\(r,a,h) -> Point.Cylindrical (r,a,-focus)) $ Point.cylindrical p

prop_radiusBecomingGreater :: Double -> Coords -> Bool 
prop_radiusBecomingGreater focus (r,_,_) = 
  ifValidInput focus r (r <= r') where
    r' = reversedRadiusTransformation (Paraboloid.Paraboloid focus) r

check_saving_distance = Check.quickCheck $ Check.withMaxSuccess 10000 prop where
    prop = (prop_savingDistance 5) 

check_radius_growing = Check.quickCheck $ Check.withMaxSuccess 10000 prop_radiusBecomingGreater where


fst3 (x,_,_) = x
--hole = undefined