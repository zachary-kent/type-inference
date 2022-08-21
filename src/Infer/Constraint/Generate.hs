{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Infer.Constraint.Generate
  ( ConstraintState,
    index,
    constraints,
    decorate,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState, StateT (runStateT))
import Data.Map (Map)
import qualified Data.Map as Map
import Infer.Term (pattern (:$:), pattern (:=>))
import qualified Infer.Term as Term
import qualified Infer.Term.Decorated as Term.Decorated
import qualified Infer.Term.Decorated.Unsolved as Term.Decorated.Unsolved
import qualified Infer.Term.Undecorated as Term.Undecorated
import Infer.Type (pattern (:->))
import qualified Infer.Type as Type
import Infer.Type.Unsolved (pattern Constructor, pattern Var)
import qualified Infer.Type.Unsolved as Type.Unsolved
import Lens.Micro.Platform (makeLenses, (%=), (<<%=))

-- |  A constraint of the form @(t1, t2)@ indicating that @t1@ must be
-- equivalent to @t2@
type Constraint = (Type.Unsolved.Type, Type.Unsolved.Type)

-- | State indicating the zero-based index of the next type variable to be
-- generated, alongside a list of type constraints that must hold
data ConstraintState = ConstraintState
  { _index :: Int,
    _constraints :: [Constraint]
  }

makeLenses ''ConstraintState

-- | Generate a new type variable
gensym :: MonadState ConstraintState m => m Type.Unsolved.Type
gensym = Var <$> (index <<%= succ)

-- | Add a constraint in the form of a type equality
addConstraint :: MonadState ConstraintState m => Constraint -> m ()
addConstraint constraint = constraints %= (constraint :)

-- | A typing context
type Env = Map String Type.Unsolved.Type

-- | Either state indicating the set of type constraints with an associated
-- computation, or a string indicating an unbound variable present in some term
type Result a = StateT ConstraintState (Either String) a

-- | A @Result@ whose stateful computation computes a term decorated with
-- unsolved types
type TermResult = Result Term.Decorated.Unsolved.Term

-- | A term that is a type constructor, not a type variable
constructor ::
  Term.Decorated.Unsolved.Node ->
  Type.Unsolved.Node ->
  Term.Decorated.Unsolved.Term
constructor term typ = Term.Decorated.Term term $ Constructor typ

-- | Given a typing context and term, try to infer its type and generate an
-- associated set of constraints
generate :: Env -> Term.Undecorated.Term -> TermResult
generate env (Term.Undecorated.Term node) =
  case node of
    Term.Unit -> pure unit
    Term.Abs x e -> generateAbs env x e
    Term.App e1 e2 -> generateApp env e1 e2
    Term.Var x -> generateVar env x

-- | A decorated term of type unit
unit :: Term.Decorated.Unsolved.Term
unit = constructor Term.Unit Type.Unit

generateWithType ::
  Env ->
  Term.Undecorated.Term ->
  Result (Term.Decorated.Unsolved.Term, Type.Unsolved.Type)
generateWithType env e = do
  e'@(Term.Decorated.Term _ t) <- generate env e
  pure (e', t)

generateAbs :: Env -> String -> Term.Undecorated.Term -> TermResult
generateAbs env x e = do
  t1 <- gensym
  let env' = Map.insert x t1 env
  (e', t2) <- generateWithType env' e
  pure $ constructor (x :=> e') (t1 :-> t2)

generateApp ::
  Env -> Term.Undecorated.Term -> Term.Undecorated.Term -> TermResult
generateApp env e1 e2 = do
  (e1', t1) <- generateWithType env e1
  (e2', t2) <- generateWithType env e2
  t <- gensym
  addConstraint (t1, Constructor $ t2 :-> t)
  pure $ Term.Decorated.Term (e1' :$: e2') t

generateVar :: Env -> String -> TermResult
generateVar env x =
  case Map.lookup x env of
    Nothing -> throwError x
    Just ty -> pure $ Term.Decorated.Term (Term.Var x) ty

-- | Decorate a term with its inferred type and produce an associated set of
-- type constraints that must hold
decorate ::
  Term.Undecorated.Term ->
  Either String (Term.Decorated.Unsolved.Term, ConstraintState)
decorate e =
  runStateT (generate Map.empty e) undefined
