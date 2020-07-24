module Tiling where
import Data.Point hiding (hole)
import Data.Paraboloid
import qualified StraightTransformation --hiding (hole)
import qualified CalculationMethods.ApproximateCalculations as CalcMethod

import qualified TriangularMap.Nodes as Nodes
import TriangularMap.Nodes (Node)

tile paraboloid density =  
  wearedTriangles $ wearedTriangle (configuredTransformation paraboloid)

-- data Triangle = Triangle (Point,Point,Point) deriving (Show,Eq)

data Transformation = Transformation (Paraboloid -> Point -> Point)

wearedPoint    :: Transformation -> Paraboloid -> Point -> Point
wearedTriangle :: (Point -> Point) -> Node Point -> Node Point
wearedTriangles:: (Node Point -> Node Point) -> [Node Point] -> [Node Point]

tile :: Paraboloid -> Float -> [Node Point] -> [Node Point]

configuredTransformation :: Paraboloid -> Point -> Point

wearedPoint (Transformation f) paraboloid = 
  f paraboloid

wearedTriangle transformation = fmap transformation 

calculationsConfigurations :: StraightTransformation.FunctionReversing
calculationsConfigurations = 
  StraightTransformation.functionReversing 30 (CalcMethod.Density 13)

configuredTransformation = 
   wearedPoint ( Transformation $ StraightTransformation.transformation calculationsConfigurations)

wearedTriangles = map

--hole = undefined
