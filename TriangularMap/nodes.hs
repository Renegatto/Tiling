module TriangularMap.Nodes where

data Node a = Node a BoundedTriangles deriving (Eq,Show)
instance Functor Node where
    fmap f (Node x trgs) = Node (f x) trgs

import Data.Point

