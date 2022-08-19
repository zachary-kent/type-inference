module Inference.Term.Undecorated (Term (..)) where

import qualified Inference.Term as T

-- | An undecorated term in the lambda calculus
newtype Term = Term (T.Term Term)
