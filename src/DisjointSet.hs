module DisjointSet (DisjointSet, new, find) where

import Control.Monad.ST (ST)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import DisjointSet.Node (Node)
import qualified DisjointSet.Node as Node

-- | A collection of disjoint set
newtype DisjointSet s a = DisjointSet (Vector (Node s Int a))

-- | @new len@ is a collection of @len@ disjoint singleton subsets, wrapped
-- in the ST Monad
new :: Int -> ST s (DisjointSet s a)
new len = DisjointSet <$> Vector.generateM len Node.new

-- | Find the representative of the set containing a given key. Error if the
-- | key is out of range
find :: DisjointSet s a -> Int -> ST s (Node.Parent s Int a)
find (DisjointSet t) = Node.find . (t !)
