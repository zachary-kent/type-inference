{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Inference.Type (Type (..), Solved (..)) where

-- | Whether the constraints for a type have been fully solved
data Solved = Solved | Unsolved

-- | The type of a term in the lambda calculus
data Type a where
  -- | The unit type with only one inhabitant
  Unit :: Type a
  -- | @Fun t1 t2@ represents the function type @t1 -> t2@
  Fun :: Type a -> Type a -> Type a
  -- An unsolved type variable
  Var :: Type 'Unsolved
