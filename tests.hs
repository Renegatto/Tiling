module Tests where

data Mistake = Mistake Double

mistake :: Mistake -> Double -> Double -> Bool
mistake (Mistake m) a b = (abs $ a - b) < m

mistake3 :: Mistake -> (Double,Double,Double) -> (Double,Double,Double) -> Bool
mistake3 m (a,b,c) (a',b',c') = mistake m a a' && mistake m b b' && mistake m c c' 