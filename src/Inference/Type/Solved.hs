module Inference.Type.Solved (Type (..), Node) where

import qualified Inference.Type

-- | A fully solved type without any type variables
newtype Type = Type Node

-- | A Node in the type tree
type Node = Inference.Type.Type Type
