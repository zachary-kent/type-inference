{-# LANGUAGE FlexibleContexts #-}

module Inference.Constraint where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State (MonadState, State)
import Data.Map (Map)
import qualified Data.Map as M
import Inference.Term (Term)
import qualified Inference.Term as Term
import qualified Inference.Term.Decorated as Decorated
import qualified Inference.Term.Undecorated as Undecorated
import qualified Inference.Type as Type
import qualified Inference.Type.Solved as Solved
import qualified Inference.Type.Unsolved as Unsolved

type Constraint = (Unsolved.Type, Unsolved.Type)

data ConstraintState = ConstraintState
  { index :: Int,
    constraints :: [Constraint]
  }

-- | A typing context
type Env = Map String Unsolved.Type

type Generate t m = Env -> t -> m Decorated.Unsolved

constructorTerm :: Term Decorated.Unsolved -> Unsolved.Node ->

generate ::
  (MonadState ConstraintState m, MonadError String m) =>
  Env ->
  Undecorated.Term ->
  m Decorated.Unsolved
generate env (Undecorated.Term node) = generateNode env node
  where
    generateNode _ Term.Unit = return $ Decorated.Term Term.Unit $ Unsolved.Constructor Type.Unit
