module Tiling where
import Data.Point hiding (hole)
import Data.Paraboloid
import qualified StraightTransformation --hiding (hole)
import qualified CalculationMethods.ApproximateCalculations as CalcMethod

tile paraboloid density =  
  wearedTriangles $ wearedTriangle (configuredTransformation paraboloid)

data Triangle = Triangle (Point,Point,Point) deriving (Show,Eq)

data Transformation = Transformation (Paraboloid -> Point -> Point)

wearedPoint    :: Transformation -> Paraboloid -> Point -> Point
wearedTriangle :: (Point -> Point)       -> Triangle   -> Triangle
wearedTriangles:: (Triangle -> Triangle) -> [Triangle] -> [Triangle]

tile :: Paraboloid -> Float -> [Triangle] -> [Triangle]

configuredTransformation :: Paraboloid -> Point -> Point

wearedPoint (Transformation f) paraboloid = 
  f paraboloid

wearedTriangle transformation (Triangle (a,b,c)) = 
  Triangle (f a,f b,f c) where f = transformation

calculationsConfigurations :: StraightTransformation.FunctionReversing
calculationsConfigurations = 
  StraightTransformation.functionReversing 30 (CalcMethod.Density 13)

configuredTransformation = 
   wearedPoint ( Transformation $ StraightTransformation.transformation calculationsConfigurations)

wearedTriangles = map

--hole = undefined
