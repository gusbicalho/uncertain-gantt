{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Agent.Class (
  Agent (..),
  NewAgent (..),
  RunAction (..),
  RunNamedAction (..),
  AgentOn,
) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

type AgentOn agent action m =
  ( Agent agent
  , m ~ AgentMonad agent
  , RunAction action agent
  )

-- | An Agent run on some specific monad
class
  (Monad (AgentMonad runner)) =>
  Agent runner
  where
  type AgentMonad runner :: Type -> Type

-- | An Agent may have a way to build an initial state
class
  Agent runner =>
  NewAgent runner
  where
  initial :: AgentMonad runner runner

-- | An agent accepts actions which transform its state
class
  Agent runner =>
  RunAction action runner
  where
  run :: action -> runner -> AgentMonad runner runner

-- | An agent accepts named actions which transform its state
class
  Agent runner =>
  RunNamedAction (label :: Symbol) action runner
  where
  runNamed :: action -> runner -> AgentMonad runner runner
