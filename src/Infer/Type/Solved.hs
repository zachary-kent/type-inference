module Infer.Type.Solved (Type (..), Node) where

import qualified Infer.Type

-- | A fully solved type without any type variables
newtype Type = Type Node

-- | A Node in the type tree
type Node = Infer.Type.Type Type
