module Inference (Error (..), decorate) where

import qualified Inference.Term.Decorated as Dec
import Inference.Term.Undecorated (Term)
import qualified Inference.Type.Solved as Solved
import qualified Inference.Type.Unsolved as Unsolved

-- | An error that can arise from a type inference failure
data Error
  = -- | An unbound variable
    Unbound String
  | -- | @Mismatch t1 t2@ indicates @t1@ cannot be unified with @t2@
    Mismatch (Dec.Term Unsolved.Type) (Dec.Term Unsolved.Type)
  | -- | A circular constraint, e.g. @t = t -> t@
    Circular (Dec.Term Unsolved.Type) (Dec.Term Unsolved.Type)

-- | Decorate each term in an AST with its inferred type, or produce an error
decorate :: Term -> Either Error (Dec.Term Solved.Type)
decorate = undefined
