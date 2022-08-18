{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Inference.Type
  ( Type,
    Node (..),
    Solved (..),
    Var (..),
  )
where

import qualified Data.Kind

-- | Whether a type has been solved for
data Solved = Solved | Unsolved

-- | The type of a term in the lambda calculus
type Type :: Solved -> Data.Kind.Type
type family Type a where
  Type 'Solved = Node 'Solved
  Type 'Unsolved = Var

data Node a
  = -- | The unit type with only one inhabitant
    Unit
  | -- | @t1 :-> t2@ represents the function type @t1 -> t2@
    Type a :-> Type a

deriving instance (Eq (Type a)) => Eq (Node a)

deriving instance (Eq (Type 'Unsolved))

-- | A type containing type variables
data Var
  = -- | A type variable
    Var Int
  | -- | A (possibly nullary) type constructor
    Constructor (Node 'Unsolved)
