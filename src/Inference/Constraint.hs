{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Inference.Constraint where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State (MonadState, State, gets, modify')
import Data.Map (Map)
import qualified Data.Map as Map
import Inference.Term (Term, pattern (:=>))
import qualified Inference.Term as Term
import qualified Inference.Term.Decorated as Decorated
import qualified Inference.Term.Decorated.Solved as Term.Solved
import qualified Inference.Term.Decorated.Unsolved as Term.Decorated.Unsolved
import qualified Inference.Term.Undecorated as Undecorated
import Inference.Type (pattern (:->))
import qualified Inference.Type as Type
import qualified Inference.Type.Solved as Type.Solved
import qualified Inference.Type.Unsolved as Type.Unsolved
import Lens.Micro.Platform

type Constraint = (Type.Unsolved.Type, Type.Unsolved.Type)

data ConstraintState = ConstraintState
  { _index :: Int,
    _constraints :: [Constraint]
  }

makeLenses ''ConstraintState

gensym :: MonadState ConstraintState m => m Type.Unsolved.Type
gensym = Type.Unsolved.Var <$> (index <<%= succ)

addConstraint :: MonadState ConstraintState m => Constraint -> m ()
addConstraint constraint = modify' $ over constraints (constraint :)

-- | A typing context
type Env = Map String Type.Unsolved.Type

-- | A term that is a type constructor, not a type variable
constructorTerm ::
  Term.Decorated.Unsolved.Node ->
  Type.Unsolved.Node ->
  Term.Decorated.Unsolved.Term
constructorTerm term typ =
  Decorated.Term term $ Type.Unsolved.Constructor typ

generate ::
  (MonadState ConstraintState m, MonadError String m) =>
  Env ->
  Undecorated.Term ->
  m Term.Decorated.Unsolved.Term
generate env (Undecorated.Term node) = generateNode env node
  where
    generateNode _ Term.Unit = pure unit
    generateNode env (Term.Abs param body) = undefined

unit :: Term.Decorated.Unsolved.Term
unit = constructorTerm Term.Unit Type.Unit

generateAbs ::
  (MonadState ConstraintState m, MonadError String m) =>
  Env ->
  String ->
  Undecorated.Term ->
  m Term.Decorated.Unsolved.Term
generateAbs env x e = do
  t1 <- gensym
  let env' = Map.insert x t1 env
  e'@(Decorated.Term _ t2) <- generate env' e
  pure $ constructorTerm (x :=> e') (t1 :-> t2)
