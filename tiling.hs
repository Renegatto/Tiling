module Tiling where
import Point hiding (hole)
import Paraboloid

tile paraboloid =  
  wearedTriangles $ wearedTriangle (configuredTransformation paraboloid)

data Triangle = Triangle (Point,Point,Point) deriving (Show,Eq)

data Compensation = Compensation (Paraboloid -> Point -> Point)
data Projection   = Projection   (Paraboloid -> Point -> Point)

compensation parab p = Cylindrical (r,a,h) where
  (r,a,h) = hole
projection   parab p = Cylindrical (r,a,h) where
  (r,a,h) = hole

wearedPoint    :: (Compensation, Projection) -> Paraboloid -> Point -> Point
wearedTriangle :: (Point -> Point)       -> Triangle   -> Triangle
wearedTriangles:: (Triangle -> Triangle) -> [Triangle] -> [Triangle]

tile :: Paraboloid -> [Triangle] -> [Triangle]

configuredTransformation :: Paraboloid -> Point -> Point

wearedPoint (Compensation f, Projection g) paraboloid = 
  g paraboloid . f paraboloid

wearedTriangle transformation (Triangle (a,b,c)) = 
  Triangle (f a,f b,f c) where f = transformation

configuredTransformation = 
  wearedPoint (Compensation compensation, Projection projection)
wearedTriangles = map

hole = undefined
