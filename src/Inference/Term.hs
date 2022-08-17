{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Inference.Term
  ( Term (..),
    Typing (..),
    Node,
  )
where

import Data.Kind (Type)
import qualified Inference.Type

-- | Whether the lambda term is typed or untyped
data Typing = Typed | Untyped

-- | @Node state@ is the the type of a node in the AST given state @state@
type Node :: Typing -> Type
type family Node a where
-- An untyped node is just the term itself
  Node 'Untyped = Term 'Untyped
-- A typed node is the term tagged with its corresponding type
  Node 'Typed = (Term 'Typed, Inference.Type.Type)

-- | A term in the lambda calculus
type Term :: Typing -> Type
data Term a
  = -- | The only inhabitant of the unit type, ()
    Unit
  | -- | Abstraction
    Abs String (Node a)
  | -- | Application
    App (Node a) (Node a)
  | -- | Variable
    Var String
