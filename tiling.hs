module Tiling where
import Point hiding (hole)
import Paraboloid

tile paraboloid =  
  wearedTriangles $ wearedTriangle (configuredTransformation paraboloid)

data Triangle = Triangle (Point,Point,Point) deriving (Show,Eq)

data Transformation = Transformation (Paraboloid -> Point -> Point)

transformation parab p = Cylindrical (r,a,h) where
  (r,a,h) = hole


wearedPoint    :: Transformation -> Paraboloid -> Point -> Point
wearedTriangle :: (Point -> Point)       -> Triangle   -> Triangle
wearedTriangles:: (Triangle -> Triangle) -> [Triangle] -> [Triangle]

tile :: Paraboloid -> [Triangle] -> [Triangle]

configuredTransformation :: Paraboloid -> Point -> Point

wearedPoint (Transformation f) paraboloid = 
  f paraboloid


wearedTriangle transformation (Triangle (a,b,c)) = 
  Triangle (f a,f b,f c) where f = transformation

configuredTransformation = 
  wearedPoint (Transformation transformation)
wearedTriangles = map

hole = undefined
