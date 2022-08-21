module Infer.Type.Unsolved (Type (..), Node) where

import qualified Infer.Type

-- A type containing type variables
data Type
  = -- | A type variable
    Var Int
  | -- | A (possibly nullary) type constructor
    Constructor Node

-- | A node in the type tree
type Node = Infer.Type.Type Type
