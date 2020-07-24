module Data.Point where
import Control.Arrow

data Point =
  Cartesian (Double,Double,Double)
  |Cylindrical (Double,Double,Double) deriving (Show,Eq)

--hole = undefined

class Point3 p where
  cartesian :: p -> (Double,Double,Double)
  cylindrical :: p -> (Double,Double,Double)
  angle :: p -> Double
  height :: p -> Double
  radius :: p -> Double
  distance :: p -> p -> Double
  map_x :: (Double -> Double) -> p -> p
  map_y :: (Double -> Double) -> p -> p
  map_z :: (Double -> Double) -> p -> p

  map_r :: (Double -> Double) -> p -> p
  map_a :: (Double -> Double) -> p -> p
  map_h :: (Double -> Double) -> p -> p
instance Point3 Point where

  cartesian (Cartesian p) = p
  cartesian (Cylindrical (r,a,h)) = (x,y,z) where 
      x = r_xy * cos a
      y = r_xy * sin a
      z = h
      r_xy = sqrt (r**2-z**2) -- <== r_xy^2 + z^2 = r^2

  cylindrical (Cartesian (x,y,z)) = (r,a,h) where
      r = sqrt (x**2 + y**2 + z**2)
      a = angle_from (zero_or $ x/r_xy) (zero_or $ y/r_xy)
      h = z
      r_xy = sqrt (r**2-z**2)

  cylindrical (Cylindrical p) = p

  angle  (Cylindrical (_,a,_)) = a
  angle cart = angle $ Cylindrical $ cylindrical cart

  height (Cylindrical (_,_,h)) = h
  height cart = height $ Cylindrical $ cylindrical cart

  radius (Cylindrical (r,_,_ )) = r
  radius cart = radius $ Cylindrical $ cylindrical cart

  distance p0 p1 = 
    sqrt (x**2 + y**2 + z**2) where
      ((x0,y0,z0),(x1,y1,z1)) = cartesian *** cartesian $ (p0,p1)
      (x,y,z) = (x0-x1,y0-y1,z0-z1)

  map_x f (Cartesian (x,y,z)) = Cartesian (f x,y,z)
  map_x f p = map_x f (Cartesian $ cartesian p)

  map_y f (Cartesian (x,y,z)) = Cartesian (x,f y,z)
  map_y f p = map_y f (Cartesian $ cartesian p)

  map_z f (Cartesian (x,y,z)) = Cartesian (x,y,f z)
  map_z f (Cylindrical (r,a,h)) = Cylindrical (r,a,f h)
  
  map_r f (Cylindrical (r,a,h)) = Cylindrical (f r,a,h)
  map_r f p = map_r f (Cylindrical $ cylindrical p)

  map_a f (Cylindrical (r,a,h)) = Cylindrical (r,f a,h)
  map_a f p = map_a f (Cylindrical $ cylindrical p)

  map_h = map_z
angle_from :: Double -> Double -> Double
angle_from cosa sina = 
    case (right_halfcircle,upper_halfcircle) of
        (True,True) -> 0 + a
        (True,False) -> 2 * pi + a
        (False,False) -> pi - a
        (False,True) -> pi - a
    where
        a = asin sina
        right_halfcircle = cosa > 0
        upper_halfcircle = sina > 0

zero_or :: Double -> Double
zero_or x = if x == x then x else 0 -- because NaN /= NaN

{-
radiansInDegrees = 180/pi

rad2deg :: Double -> Double
rad2deg = (* radiansInDegrees)
deg2rad :: Double -> Double
deg2rad = (/ radiansInDegrees)
zero_or :: Double -> Double
zero_or x = if x == x then x else 0 -}