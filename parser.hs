{-# LANGUAGE OverloadedStrings #-}

module Parser where
import Data.Aeson as JSON
import qualified TriangularMap.Nodes as Nodes
import TriangularMap.Nodes(Node)
import qualified TriangularMap.Triangles as Triangles
import qualified Data.Point as Point
import Data.Point (Point)

instance ToJSON Point where
    toJSON p = JSON.object ["x" .= x, "y" .= y, "z" .= z] where
        (x,y,z) = Point.cartesian p
instance ToJSON Triangles.BoundedTriangles where
    toJSON (Triangles.BoundedTriangles xs) = JSON.object ["boundedTriangles" .= xs]

instance (ToJSON a) => ToJSON (Node a) where
    toJSON (Nodes.Node x trgs) = JSON.object [
        "Node" .= JSON.object 
            [
                "data" .= x,
                "triangles" .= trgs
            ]
        
        ]

--nodesToOutput