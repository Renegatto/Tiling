module Tests.SwitchingCoordinates where

import Data.Either as Either
import Data.Either (Either)
import qualified Test.QuickCheck as Check
import Tests hiding (mistake)

import qualified Paraboloid
import Paraboloid (Paraboloid)
import qualified Point
import Point (Point)

type Coords = (Double,Double,Double)

identitySwitch :: Mistake -> Point -> ((Coords,Coords),(Coords,Coords))
identitySwitch m point = (cyl_pair, cart_pair) where
  cyl  = (Point.cylindrical $ Point.Cartesian   $ Point.cartesian   point)
  cart = (Point.cartesian   $ Point.Cylindrical $ Point.cylindrical point)
  (cyl_pair, cart_pair) = ((Point.cylindrical point,cyl),(Point.cartesian point,cart))

identitySwitchingResult :: 
  Mistake 
  -> Coords
  -> Either (Mistake,Coords,Coords,Coords,Coords) (Mistake,Coords)
identitySwitchingResult m point =
  case is_success of
    False -> Either.Left  (m,point,res0,p1,res1)
    True  -> Either.Right (m,point)
  where
    is_success = (mistake3 m p0 res0) && (mistake3 m p1 res1)
    ((p0,res0),(p1,res1)) = identitySwitch m (Point.Cartesian point)

prop_isReversable :: Mistake -> Coords -> Bool
prop_isReversable mist = Either.isRight . identitySwitchingResult mist

check_is_reversable = Check.quickCheck $ Check.withMaxSuccess 100 prop where
    prop = prop_isReversable (Mistake 0.0006)