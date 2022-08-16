module Inference.Term (Term (..)) where

-- | A term in the lambda calculus
data Term
  = -- | The only inhabitant of the unit type, ()
    Unit
  | -- | Abstraction
    Abs String Term
  | -- | Application
    App Term Term
  | -- | Variable
    Var String
