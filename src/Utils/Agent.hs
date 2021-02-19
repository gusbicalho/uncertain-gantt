{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Agent (
  Agent (..),
  RunAction (..),
  AgentOn (..),
  AgentRunsActionAsVariant,
) where

import Data.Kind (Type)
import Data.Row.Dictionaries (Unconstrained1)
import qualified Data.Row.Variants as Variants
import GHC.TypeLits (Symbol)
import qualified Utils.Runner as Runner

-- | An Agent has an initial state, and accepts actions which transform its state
class
  (Monad (AgentMonad runner)) =>
  Agent runner
  where
  type AgentMonad runner :: Type -> Type
  initial :: AgentMonad runner runner

class
  Agent runner =>
  RunAction (label :: Symbol) message runner
  where
  runAction :: message -> runner -> AgentMonad runner runner

-- | A wrapper that allows an Agent to accept Runner requests
newtype AsRunner runner = AsRunner {unAsRunner :: runner}

instance Agent runner => Agent (AsRunner runner) where
  type AgentMonad (AsRunner runner) = AgentMonad runner
  initial = AsRunner <$> initial
  {-# INLINE initial #-}

instance RunAction label message runner => RunAction label message (AsRunner runner) where
  runAction message (AsRunner runner) = AsRunner <$> runAction @label message runner
  {-# INLINE runAction #-}

instance
  ( RunAction (label :: Symbol) message (AsRunner runner)
  , m ~ AgentMonad runner
  ) =>
  Runner.Run (AsRunner runner) m (Runner.Named label message) (AsRunner runner)
  where
  run runner (Runner.Named msg) = runAction @label msg runner
  {-# INLINE run #-}

data AgentOn action m where
  AgentOn :: AgentRunsActionAsVariant action m agent => agent -> AgentOn action m

instance
  (Monad m, Variants.FromNative action) =>
  Runner.Run (AgentOn action m) m action (AgentOn action m)
  where
  run (AgentOn (agent :: agent)) message =
    AgentOn . Variants.erase @((~) (AsRunner agent)) unAsRunner
      <$> Runner.runVariant (AsRunner agent) (Variants.fromNative message)

type AgentRunsActionAsVariant action m agent =
  ( Agent agent
  , m ~ AgentMonad agent
  , Runner.RunVariant
      (AsRunner agent)
      m
      (Variants.NativeRow action)
      (Runner.UniformRow (AsRunner agent) (Variants.NativeRow action))
  , Variants.FromNative action
  , Variants.Forall
      (Variants.NativeRow action)
      Unconstrained1
  , Variants.Forall
      ( Runner.UniformRow
          (AsRunner agent)
          (Variants.NativeRow action)
      )
      ((~) (AsRunner agent))
  )
