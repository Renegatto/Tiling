module ReversedTransformation where

import qualified Data.Point as Point hiding (hole)
import Data.Point(Point)
import Data.Paraboloid

type RadiusOnParaboloid = Double
type RadiusOnPlane = Double
type ParabFocus = Double
type Radius = Double

reversedPointTransformation :: Paraboloid -> Point -> Point
reversedPointTransformation parab p = 
  Point.Cylindrical (reversedRadiusTransformation parab r, a, h) where
    (r,a,h) = Point.cylindrical p

-- focus,radius must be non-zero positive
reversedRadiusTransformation :: Paraboloid -> RadiusOnParaboloid -> RadiusOnPlane
reversedRadiusTransformation (Paraboloid focus) = radius' focus

-- f = focus, radius' - new horisontal radius of point, r - previous hor radius
{-@ radius' :: {f:Double | f > 0} -> {r:Double | r >= 0} -> {r':Double | r' >= 0 >= r} @-}
radius' :: ParabFocus -> Radius -> Radius
radius' f r = (1/4/f) * big_expr - f * (log (2*f)) where
  big_expr = (r * scary_root) +  (4 * f**2) * (log . abs) (r + scary_root)
  scary_root = sqrt (r**2 + 4*f**2)