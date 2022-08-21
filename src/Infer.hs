module Infer (Error (..), decorate) where

import qualified Infer.Term.Decorated.Solved as Term.Decorated.Solved
import qualified Infer.Term.Undecorated as Term.Undecorated
import qualified Infer.Type.Unsolved as Type.Unsolved

-- | An error that can arise from a type inference failure
data Error
  = -- | An unbound variable
    Unbound String
  | -- | @Mismatch t1 t2@ indicates @t1@ cannot be unified with @t2@
    Mismatch Type.Unsolved.Type Type.Unsolved.Type
  | -- | A circular constraint, e.g. @t = t -> t@
    Circular Type.Unsolved.Type Type.Unsolved.Type

-- | Decorate each term in an AST with its inferred type, or produce an error
decorate :: Term.Undecorated.Term -> Either Error Term.Decorated.Solved.Term
decorate = undefined
