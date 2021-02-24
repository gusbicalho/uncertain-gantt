{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Agent.Class (
  Agent (..),
  RunAction (..),
) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

-- | An Agent has an initial state
class
  (Monad (AgentMonad runner)) =>
  Agent runner
  where
  type AgentMonad runner :: Type -> Type
  initial :: AgentMonad runner runner

-- | An agent accepts named actions which transform its state
class
  Agent runner =>
  RunAction (label :: Symbol) action runner
  where
  runAction :: action -> runner -> AgentMonad runner runner
