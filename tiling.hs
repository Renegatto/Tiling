module Tiling where
import Point hiding (hole)
import Paraboloid
import qualified StraightTransformation --hiding (hole)

tile paraboloid density =  
  wearedTriangles $ wearedTriangle (configuredTransformation density paraboloid)

data Triangle = Triangle (Point,Point,Point) deriving (Show,Eq)

data Transformation = Transformation (Paraboloid -> Point -> Point)

transformation parab density p = Cylindrical (r,a,h) where
  (r,a,h) = hole


wearedPoint    :: Transformation -> Paraboloid -> Point -> Point
wearedTriangle :: (Point -> Point)       -> Triangle   -> Triangle
wearedTriangles:: (Triangle -> Triangle) -> [Triangle] -> [Triangle]

tile :: Paraboloid -> Float -> [Triangle] -> [Triangle]

configuredTransformation :: Float -> Paraboloid -> Point -> Point

wearedPoint (Transformation f) paraboloid = 
  f paraboloid


wearedTriangle transformation (Triangle (a,b,c)) = 
  Triangle (f a,f b,f c) where f = transformation

configuredTransformation density = 
   wearedPoint ( Transformation $ undefined {- StraightTransformation.transformation  -} density)

wearedTriangles = map

hole = undefined
