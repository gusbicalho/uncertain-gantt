{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Utils.Agent.Generic (
  Agent (..),
  RunAction (..),
  AgentOn (..),
  RunsActionGenerically,
  run,
) where

import Data.Kind (Type)
import Utils.Agent.Class (Agent (..), RunAction (..))
import Utils.GenericVisitor (CanVisit, GenericVisitor, VisitNamed (..), VisitorResult, visit)

{- |
  Existential Agent type, hiding implementation for handling all cases of a
  certain sum type.
-}
data AgentOn action (m :: Type -> Type) where
  AgentOn :: RunsActionGenerically action m agent => agent -> AgentOn action m

-- | A wrapper that allows an Agent to act as a GenericVisitor
newtype AsGV agent = AsGV {unAsGV :: agent}

{- |
  An Agent for an action is a thing that, if wrapped AsGV, implements a
  GenericVisitor for that action, always retuning the new state of the agent.

  This implementation is given by instances defined in this module, which
  delegate work to the custom instances for RunAction defined for the `agent`
  type.
-}
type RunsActionGenerically action m agent =
  ( AsGV agent `CanVisit` action
  , VisitorResult (AsGV agent) ~ m (AsGV agent)
  )

-- | An AgentOn an action can run that action, returning the Agent's new state
run :: Functor m => AgentOn action m -> action -> m (AgentOn action m)
run (AgentOn (agent :: agent)) action =
  AgentOn . unAsGV <$> visit (AsGV agent) action

instance Agent runner => GenericVisitor (AsGV runner) where
  type VisitorResult (AsGV runner) = AgentMonad runner (AsGV runner)

instance
  RunAction label action runner =>
  VisitNamed label action (AsGV runner)
  where
  visitNamed (AsGV r) msg = AsGV <$> runAction @label msg r
  {-# INLINE visitNamed #-}
