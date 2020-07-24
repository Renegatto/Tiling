module Data.Triangle where
--import TriangularMap (BoundedTriangles)

heightFromSideLength :: Double -> Double
heightFromSideLength = (sqrt (3/4) *) 

class Equilateral t where
    height  :: t -> Double
    side    :: t -> Double


