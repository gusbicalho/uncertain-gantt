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
  RunsActionGenerically,
  runGeneric,
) where

import Utils.Agent.Class (Agent (..), RunAction (..))
import Utils.GenericVisitor (CanVisit, GenericVisitor (..), VisitNamed (..), visit)

runGeneric ::
  forall nameTransform action m agent.
  RunsActionGenerically nameTransform action m agent =>
  agent ->
  action ->
  m agent
runGeneric agent action =
  unGenericAgent <$> visit (GenericAgent @nameTransform agent) action
{-# INLINE runGeneric #-}

-- | A wrapper that allows an Agent to act as a GenericVisitor
newtype GenericAgent nameTransform agent = GenericAgent {unGenericAgent :: agent}

{- |
  An Agent for an action is a thing that, if wrapped GenericAgent, implements a
  GenericVisitor for that action, always retuning the new state of the agent.

  This implementation is given by instances defined in this module, which
  delegate work to the custom instances for RunAction defined for the `agent`
  type.
-}
type RunsActionGenerically nameTransform action m agent =
  ( Functor m
  , GenericAgent nameTransform agent `CanVisit` action
  , VisitorResult (GenericAgent nameTransform agent) ~ m (GenericAgent nameTransform agent)
  )

instance Agent runner => GenericVisitor (GenericAgent nameTransform runner) where
  type ConstructorNameTransformSymbol (GenericAgent nameTransform runner) = nameTransform
  type VisitorResult (GenericAgent nameTransform runner) = AgentMonad runner (GenericAgent nameTransform runner)

instance
  RunAction label action runner =>
  VisitNamed label action (GenericAgent nameTransform runner)
  where
  visitNamed (GenericAgent r) msg = GenericAgent <$> runAction @label msg r
  {-# INLINE visitNamed #-}
