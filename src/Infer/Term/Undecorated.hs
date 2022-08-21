module Infer.Term.Undecorated (Term (..), Node) where

import qualified Infer.Term as T

-- | An undecorated term in the lambda calculus
newtype Term = Term Node

-- | An undecorated node
type Node = T.Term Term
