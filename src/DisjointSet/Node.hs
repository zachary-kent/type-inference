{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module DisjointSet.Node
  ( Node,
    Parent (..),
    find,
    new,
    union,
    value,
    setRoot,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.STRef
  ( STRef,
    modifySTRef',
    newSTRef,
    readSTRef,
    writeSTRef,
  )

-- | A Disjoint set node. We ensure there is only one type constructor per
-- equivalence class by only allowing type variables to have a parent
data Parent s a b
  = -- | A type variable
    Child (Node s a b)
  | -- | A type constructor
    Root b

-- | A type variable in the disjoint set
data Node s a b = Node
  { -- | The parent of this Node, if any
    parent :: STRef s (Maybe (Parent s a b)),
    -- | The rank (height) of this node
    rank :: STRef s Int,
    -- | The index of the type variable stored in this node
    value :: a
  }

-- | Construct a new disjoint
new :: a -> ST s (Node s a b)
new value = do
  parent <- newSTRef Nothing
  rank <- newSTRef 0
  return $ Node {parent, rank, value}

-- | Inserting a value into a reference containing a Maybe
replaceSTRef :: STRef s (Maybe a) -> a -> ST s ()
replaceSTRef ref = writeSTRef ref . Just

-- | Find the representative of a given node
find :: Node s a b -> ST s (Parent s a b)
find child@(Node {parent}) =
  readSTRef parent >>= \case
    Nothing -> return $ Child child
    Just root@(Root _) -> return root
    Just (Child parentNode) -> do
      root <- find parentNode
      replaceSTRef parent root
      return root

-- | Set the parent of a disjoint set node
setParent :: Node s a b -> Parent s a b -> ST s ()
setParent = replaceSTRef . parent

-- | Take the union of the the representatives of two sets6
union :: Node s a b -> Node s a b -> ST s ()
union n1@(Node {rank = r1Ref}) n2@(Node {rank = r2Ref}) = do
  r1 <- readSTRef r1Ref
  r2 <- readSTRef r2Ref
  -- node with greater rank becomes parent
  -- if ranks equal, we choose n1
  let (c, p) = if r1 < r2 then (n1, n2) else (n2, n1)
  -- if ranks equal, then the rank of n1 increases by 1
  when (r1 == r2) $ modifySTRef' r1Ref succ
  setParent c $ Child p

-- | Set the parent of a Node to a root value
setRoot :: Node s a b -> b -> ST s ()
setRoot child = setParent child . Root
