module ReversedTransformation where

import qualified Point hiding (hole)
import Point(Point)
import Paraboloid

type PointOnParaboloid = Point
type PointOnPlane = Point
type ParabFocus = Double
type Radius = Double

-- Focus must be non-zero
reversedTransformation :: Paraboloid -> PointOnParaboloid -> PointOnPlane
reversedTransformation (Paraboloid focus) point = 
  Point.Cylindrical (r',a,h) where
    r' = radius' focus r
    (r,a,h) = Point.cylindrical point 

-- f = focus, radius' - new horisontal radius of point, r - previous hor radius
{-@ radius' :: {f:Double | f /= 0} -> Double -> Double @-}
radius' :: ParabFocus -> Radius -> Radius
radius' f r = (1/4/f) * big_expr - f * (log (2*f)) where
  big_expr = (r * scary_root) +  (4 * f**2) * (log . abs) (r + scary_root)
  scary_root = sqrt (r**2 + 4*f**2)

--(1/4/x) * (y * (sqrt(y**2 + 4*x**2))  +  (4 * x**2) * ln(abs(y + (sqrt(y**2 + 4*x**2))))) - x * ln(2*x)
--(sqrt(y**2 + 4*x**2))