module Inference (Error (..), decorate) where

import qualified Inference.Term.Decorated as Decorated
import Inference.Term.Undecorated (Term)

-- | An error that can arise from a type inference failure
data Error
  = -- | An unbound variable
    Unbound String
  | -- | @Mismatch t1 t2@ indicates @t1@ cannot be unified with @t2@
    Mismatch Decorated.Unsolved Decorated.Unsolved
  | -- | A circular constraint, e.g. @t = t -> t@
    Circular Decorated.Unsolved Decorated.Unsolved

-- | Decorate each term in an AST with its inferred type, or produce an error
decorate :: Term -> Either Error Decorated.Solved
decorate = undefined
