module Infer.Type (Type (..)) where

-- | A type in the lambda calculus
data Type a
  = -- | The unit type with only one inhabitant
    Unit
  | -- | @t1 :-> t2@ represents the function type @t1 -> t2@
    a :-> a
