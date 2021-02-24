{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

import Data.Data (Proxy)
import Data.Kind (Type)
import Utils.Agent.Class (Agent (..), RunAction (..))
import Utils.GenericVisitor (CanVisit, GenericVisitor (..), VisitNamed (..), visit)

{- |
  Existential Agent type, hiding implementation for handling all cases of a
  certain sum type.
-}
data AgentOn action (m :: Type -> Type) where
  AgentOn ::
    forall action m agent nameTransform.
    RunsActionGenerically nameTransform action m agent =>
    Proxy nameTransform ->
    agent ->
    AgentOn action m

-- | A wrapper that allows an Agent to act as a GenericVisitor
newtype AsGV nameTransform agent = AsGV {unAsGV :: agent}

{- |
  An Agent for an action is a thing that, if wrapped AsGV, implements a
  GenericVisitor for that action, always retuning the new state of the agent.

  This implementation is given by instances defined in this module, which
  delegate work to the custom instances for RunAction defined for the `agent`
  type.
-}
type RunsActionGenerically nameTransform action m agent =
  ( AsGV nameTransform agent `CanVisit` action
  , VisitorResult (AsGV nameTransform agent) ~ m (AsGV nameTransform agent)
  )

-- | An AgentOn an action can run that action, returning the Agent's new state
run :: Functor m => AgentOn action m -> action -> m (AgentOn action m)
run (AgentOn (p :: Proxy nameTransform) (agent :: agent)) action =
  AgentOn p . unAsGV <$> visit (AsGV @nameTransform agent) action
{-# INLINE run #-}

instance Agent runner => GenericVisitor (AsGV nameTransform runner) where
  type ConstructorNameTransformSymbol (AsGV nameTransform runner) = nameTransform
  type VisitorResult (AsGV nameTransform runner) = AgentMonad runner (AsGV nameTransform runner)

instance
  RunAction label action runner =>
  VisitNamed label action (AsGV nameTransform runner)
  where
  visitNamed (AsGV r) msg = AsGV <$> runAction @label msg r
  {-# INLINE visitNamed #-}
