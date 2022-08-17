{-# LANGUAGE DataKinds #-}

module Inference (Error (..), decorate) where

import Inference.Term (Node, Typing (..))

-- | An error that can arise from a type inference failure
data Error = Error

-- | Decorate each term in an AST with its inferred type, or produce an error
decorate :: Node 'Untyped -> Either Error (Node 'Typed)
decorate = undefined
