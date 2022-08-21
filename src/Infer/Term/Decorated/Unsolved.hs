module Infer.Term.Decorated.Unsolved (Term, Node) where

import qualified Infer.Term.Decorated as Decorated
import qualified Infer.Type.Unsolved as Unsolved

-- | A term decorated with solved types
type Term = Decorated.Term Unsolved.Type

-- | A node in the type tree
type Node = Decorated.Node Unsolved.Type
