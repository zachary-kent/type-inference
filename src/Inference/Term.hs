{-# LANGUAGE PatternSynonyms #-}

module Inference.Term
  ( Term (..),
    pattern (:=>),
    pattern (:$:),
  )
where

-- | A term in the lambda calculus
data Term a
  = -- | The only inhabitant of the unit type, ()
    Unit
  | -- | Abstraction
    Abs String a
  | -- | Application
    App a a
  | -- | Variable
    Var String

-- | Infix abstraction pattern
pattern (:=>) :: String -> a -> Term a
pattern e1 :=> e2 = Abs e1 e2

-- | Infix application pattern
pattern (:$:) :: a -> a -> Term a
pattern e1 :$: e2 = App e1 e2
