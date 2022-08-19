module Inference.Term.Decorated (Term (..), Unsolved, Solved) where

import qualified Inference.Term as T
import qualified Inference.Type.Solved as Solved
import qualified Inference.Type.Unsolved as Unsolved

-- | A lambda calculus term with a decoration of type @a@
data Term a = Term (T.Term (Term a)) a

-- | A term decorated with unsolved type variables
type Unsolved = Term Unsolved.Type

-- | A term decorated with solved types
type Solved = Term Solved.Type
