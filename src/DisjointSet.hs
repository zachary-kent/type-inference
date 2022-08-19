module DisjointSet (DisjointSet, new, find) where

import Control.Monad.ST (ST)
import qualified Data.HashTable.ST.Basic as H
import Data.Hashable (Hashable)
import DisjointSet.Node (Node)
import qualified DisjointSet.Node as Node

newtype DisjointSet s a b = DisjointSet (H.HashTable s a (Node s a b))

-- | Create an empty disjoint set
new :: ST s (DisjointSet s a b)
new = DisjointSet <$> H.new

-- | Lookup a key, return the found value if present. If not, insert and return
-- the provided default value
lookupOrInsert :: (Eq k, Hashable k) => H.HashTable s k v -> k -> v -> ST s v
lookupOrInsert t k v' = H.mutate t k value
  where
    value Nothing = (Just v', v')
    value prev@(Just v) = (prev, v)

-- | Find the representative of the set containing a given key. If the key is
-- absent, create a singleton set for it
find :: (Eq a, Hashable a) => DisjointSet s a b -> a -> ST s (Node.Parent s a b)
find (DisjointSet t) k = do
  singleton <- Node.new k
  Node.find =<< lookupOrInsert t k singleton
