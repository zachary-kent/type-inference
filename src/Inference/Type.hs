module Inference.Type (Type (..)) where

-- | The type of a term in the lambda calculus
data Type
  = -- | The unit type with only one inhabitant
    Unit
  | -- | @Fun t1 t2@ represents the function type @t1 -> t2@
    Fun Type Type
