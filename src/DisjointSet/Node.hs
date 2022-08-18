{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module DisjointSet.Node
  ( Node (..),
    Var,
    find,
    new,
    unionVars,
    unionVarConstr,
  )
where

import Control.Monad.Except
import Control.Monad.ST
import Data.STRef
  ( STRef,
    modifySTRef',
    newSTRef,
    readSTRef,
    writeSTRef,
  )
import Inference.Type (Solved (..), Type)
import qualified Inference.Type as Type

-- | A Disjoint set node. We ensure there is only one type constructor per
-- equivalence class by only allowing type variables to have a parent
data Node s
  = -- | A type constructor
    Constructor (Type.Node 'Unsolved)
  | -- | A type variable
    VarNode (Var s)

-- | A type variable in the disjoint set
data Var s = Var
  { -- | The parent of this Node, if any
    parent :: STRef s (Maybe (Node s)),
    -- | The rank (height) of this node
    rank :: STRef s Int,
    -- | The index of the type variable stored in this node
    index :: Int
  }

-- | Construct a new disjoint
new :: Type 'Unsolved -> ST s (Node s)
new (Type.Constructor ty) = return $ Constructor ty
new (Type.Var index) = do
  parent <- newSTRef Nothing
  rank <- newSTRef 0
  return $ VarNode $ Var {parent, rank, index}

find :: Node s -> ST s (Node s)
find root@(Constructor _) = return root
find child@(VarNode (Var {parent})) =
  readSTRef parent >>= \case
    Nothing -> return child
    Just parentNode -> do
      root <- find parentNode
      writeSTRef parent $ Just root
      return root

unionVars :: Var s -> Var s -> ST s ()
unionVars n1@(Var {rank = r1Ref}) n2@(Var {rank = r2Ref}) = do
  r1 <- readSTRef r1Ref
  r2 <- readSTRef r2Ref
  -- node with greater rank becomes parent
  -- if ranks equal, we choose n1
  let (c, p) = if r1 < r2 then (n1, n2) else (n2, n1)
  -- if ranks equal, then the rank of n1 increases by 1
  when (r1 == r2) $ modifySTRef' r1Ref succ
  writeSTRef (parent c) $ Just $ VarNode p

unionVarConstr :: Var s -> Type.Node 'Unsolved -> ST s ()
unionVarConstr Var {parent} typ =
  writeSTRef parent $ Just $ Constructor typ
