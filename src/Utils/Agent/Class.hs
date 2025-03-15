{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Agent.Class (
  Agent (..),
  NewAgent (..),
  RunAction (..),
  AgentOn,
) where

import Data.Kind (Type)

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
  (Agent agent) =>
  NewAgent agent
  where
  initial :: AgentMonad agent agent

-- | An agent accepts actions which transform its state
class
  (Agent agent) =>
  RunAction action agent
  where
  run :: action -> agent -> AgentMonad agent agent
