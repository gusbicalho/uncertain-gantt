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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Agent (
  Agent (..),
  RunAction (..),
  AgentOn (..),
  AgentRunsActionGenerically,
) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Utils.GenericVisitor (CanVisit, GenericVisitor (..), VisitNamed (..), visit)
import qualified Utils.Runner as Runner

{- |
  Existential Agent type, hiding implementation for handling all cases of a
  certain sum type.
-}
data AgentOn action (m :: Type -> Type) where
  AgentOn :: AgentRunsActionGenerically action m agent => agent -> AgentOn action m

{- |
  An Agent for an action is a thing that, if wrapped AsRunner, implements a
  GenericVisitor for that action, always retuning the new state of the agent.

  This implementation is given by instances defined in this module, which
  delegate work to the custom instances for RunAction defined for the `agent`
  type.
-}
type AgentRunsActionGenerically action m agent =
  ( AsRunner agent `CanVisit` action
  , VisitorResult (AsRunner agent) ~ m (AsRunner agent)
  )

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

instance GenericVisitor (AsRunner runner) where
  type VisitorResult (AsRunner runner) = AgentMonad (AsRunner runner) (AsRunner runner)

instance
  RunAction label message (AsRunner runner) =>
  VisitNamed label message (AsRunner runner)
  where
  visitNamed r msg = runAction @label msg r

instance
  Monad m =>
  Runner.Run (AgentOn action m) m action (AgentOn action m)
  where
  run (AgentOn (agent :: agent)) message =
    AgentOn . unAsRunner <$> visit (AsRunner agent) message
