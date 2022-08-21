module Infer.Term.Decorated.Solved (Term, Node) where

import qualified Infer.Term.Decorated as Decorated
import qualified Infer.Type.Solved as Solved

-- | A term decorated with solved types
type Term = Decorated.Term Solved.Type

-- | A node in the type tree
type Node = Decorated.Node Solved.Type
