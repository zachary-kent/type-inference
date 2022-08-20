module Inference.Term.Decorated (Term (..), Node) where

import qualified Inference.Term as T

-- | A lambda calculus term with a decoration of type @a@
data Term a = Term (Node a) a

-- | A node in the type tree
type Node a = T.Term (Term a)
