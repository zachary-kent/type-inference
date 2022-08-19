module Inference.Term.Decorated (Term (..)) where

import qualified Inference.Term as T

-- | A lambda calculus term with a decoration of type @a@
data Term a = Term (T.Term (Term a)) a
