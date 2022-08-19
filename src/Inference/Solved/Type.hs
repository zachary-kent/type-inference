module Inference.Solved.Type (Type (..)) where

import qualified Inference.Type

-- | A fully solved type without any type variables
newtype Type = Type (Inference.Type.Type Type)
