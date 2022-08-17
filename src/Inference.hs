{-# LANGUAGE DataKinds #-}

module Inference (Error (..), decorate) where

import Inference.Term (Node, Typing (..))
import Inference.Type (Solved (..), Type)

-- | An error that can arise from a type inference failure
data Error
  = -- | An unbound variable
    Unbound String
  | -- | @Mismatch t1 t2@ indicates @t1@ cannot be unified with @t2@
    Mismatch (Type 'Solved) (Type 'Solved)
  | -- | A circular constraint, e.g. @t = t -> t@
    Circular (Type 'Solved) (Type 'Solved)

-- | Decorate each term in an AST with its inferred type, or produce an error
decorate :: Node 'Untyped -> Either Error (Node ('Typed 'Solved))
decorate = undefined
