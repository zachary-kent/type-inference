{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Inference.Term
  ( Term (..),
    Typing (..),
    Node,
  )
where

import qualified Data.Kind
import Inference.Type (Solved (..), Type, Var)

-- | Whether the lambda term is typed or untyped
data Typing = Typed Solved | Untyped

-- | @Node state@ is the the type of a node in the AST given state @state@
type Node :: Typing -> Data.Kind.Type
type family Node a where
-- An untyped node is just the term itself
  Node 'Untyped = Term 'Untyped
-- A typed node is the term tagged with its corresponding type
  Node ('Typed 'Solved) = (Term ('Typed 'Solved), Type 'Solved)
-- If some constraints are unsolved, may be tagged with some type variables
  Node ('Typed 'Unsolved) = (Term ('Typed 'Unsolved), Var)

-- | A term in the lambda calculus
data Term a
  = -- | The only inhabitant of the unit type, ()
    Unit
  | -- | Abstraction
    Abs String (Node a)
  | -- | Application
    App (Node a) (Node a)
  | -- | Variable
    Var String
