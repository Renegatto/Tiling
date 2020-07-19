module Paraboloid where
import Point hiding (hole)

data Paraboloid = Paraboloid Double deriving (Show,Eq)

paraboloid :: Double -> Double -> Double -> Double
paraboloid x y focus = (x**2 + y**2) / 4 * focus- focus

onParaboloid :: Paraboloid -> Point -> Point
onParaboloid (Paraboloid focus) p = 
  Cartesian (x,y,z') where
    (x,y,_) = cartesian p
    z' = paraboloid x y focus