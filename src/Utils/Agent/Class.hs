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
  (Monad (AgentMonad agent)) =>
  Agent agent
  where
  type AgentMonad agent :: Type -> Type

-- | An Agent may have a way to build an initial state
class
  Agent agent =>
  NewAgent agent
  where
  initial :: AgentMonad agent agent

-- | An agent accepts actions which transform its state
class
  Agent agent =>
  RunAction action agent
  where
  run :: action -> agent -> AgentMonad agent agent

-- | An agent accepts named actions which transform its state
class
  Agent agent =>
  RunNamedAction (label :: Symbol) action agent
  where
  runNamed :: action -> agent -> AgentMonad agent agent
