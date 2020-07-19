module StraightTransformation where
{-}
type Radius = Double
type Distance  = Float
type Density   = Int
type ReverseTransform = (Paraboloid -> Point -> Point)

data DiscreteStraightTransformation = DiscreteStraightTransformation [(Radius,Radius)]
-- DiscreteStraightTransformation : Radius -> Radius as in math definition (as cartesian product)
-- But in this case it's not total


transformation :: Distance -> Density -> Paraboloid -> ReverseTransform -> [Point, Point] -}



