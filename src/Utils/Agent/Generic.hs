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
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Utils.Agent.Generic (
  RunsActionGenerically,
  GenericAgent (..),
) where

import Data.Kind (Type)
import GHC.TypeLits (AppendSymbol, Symbol)
import Utils.Agent.Class (Agent (..), NewAgent (..), RunAction (..), RunNamedAction (..))
import Utils.GenericVisitor (CanVisit, GenericVisitor (..), QualifiedName, VisitNamed (..), visit)
import Utils.QualifiedName (QualifiedNameToActionName)

-- | A wrapper that allows an Agent to act as a GenericVisitor
newtype GenericAgent nameTransform agent = GenericAgent {unGenericAgent :: agent}

instance Agent agent => Agent (GenericAgent nameTransform agent) where
  type AgentMonad (GenericAgent nameTransform agent) = AgentMonad agent

instance
  NewAgent agent =>
  NewAgent (GenericAgent nameTransform agent)
  where
  initial = GenericAgent @nameTransform <$> initial

instance
  ( RunsActionGenerically nameTransform action agent
  ) =>
  RunAction action (GenericAgent nameTransform agent)
  where
  run action agent = visit agent action
  {-# INLINE run #-}

{- |
  An Agent for an action is a thing that, if wrapped GenericAgent, implements a
  GenericVisitor for that action, always retuning the new state of the agent.

  This implementation is given by instances defined in this module, which
  delegate work to the custom instances for RunNamedAction defined for the `agent`
  type.
-}
type RunsActionGenerically nameTransform action agent =
  ( Agent agent
  , GenericAgent nameTransform agent `CanVisit` action
  , VisitorResult (GenericAgent nameTransform agent) ~ AgentMonad agent (GenericAgent nameTransform agent)
  )

instance Agent agent => GenericVisitor (GenericAgent nameTransform agent) where
  type VisitorResult (GenericAgent nameTransform agent) = AgentMonad agent (GenericAgent nameTransform agent)

instance
  ( actionLabel ~ QualifiedNameToActionName nameTransform qualifiedName
  , RunNamedAction actionLabel action agent
  ) =>
  VisitNamed
    qualifiedName
    action
    (GenericAgent nameTransform agent)
  where
  visitNamed (GenericAgent r) msg = GenericAgent <$> runNamed @actionLabel msg r
  {-# INLINE visitNamed #-}
