module Inference.Type.Unsolved (Type (..)) where

import qualified Inference.Type

-- A type containing type variables
data Type
  = -- | A type variable
    Var Int
  | -- | A (possibly nullary) type constructor
    Constructor (Inference.Type.Type Type)
