module Inference.Unsolved.Type (Type (..), Node) where

import qualified Inference.Type

-- A type containing type variables
data Type
  = -- | A type variable
    Var Int
  | -- | A (possibly nullary) type constructor
    Constructor Node

-- | A node in the type tree
type Node = Inference.Type.Type Type
