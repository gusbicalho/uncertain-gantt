{-# LANGUAGE AllowAmbiguousTypes #-}
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
  RunsActionGenerically,
  runGeneric,
) where

import Utils.Agent.Class (Agent (..), RunAction (..))
import Utils.GenericVisitor (CanVisit, GenericVisitor (..), VisitNamed (..), visit)

runGeneric ::
  forall nameTransform action m agent.
  ( Functor m
  , RunsActionGenerically nameTransform action m agent
  ) =>
  agent ->
  action ->
  m agent
runGeneric agent action =
  unAsGV <$> visit (AsGV @nameTransform agent) action
{-# INLINE runGeneric #-}

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

instance Agent runner => GenericVisitor (AsGV nameTransform runner) where
  type ConstructorNameTransformSymbol (AsGV nameTransform runner) = nameTransform
  type VisitorResult (AsGV nameTransform runner) = AgentMonad runner (AsGV nameTransform runner)

instance
  RunAction label action runner =>
  VisitNamed label action (AsGV nameTransform runner)
  where
  visitNamed (AsGV r) msg = AsGV <$> runAction @label msg r
  {-# INLINE visitNamed #-}
