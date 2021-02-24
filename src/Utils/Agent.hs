{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Utils.Agent (
  Agent (..),
  RunAction (..),
  AgentOn (..),
  AgentRunsGenerically,
  run,
) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Symbol)
import Utils.GenericVisitor qualified as GV

{- |
  Existential Agent type, hiding implementation for handling all cases of a
  certain sum type.
-}
data AgentOn action (m :: Type -> Type) where
  AgentOn :: AgentRunsGenerically action m agent => agent -> AgentOn action m

{- |
  An Agent for an action is a thing that, if wrapped AsGV, implements a
  GenericVisitor for that action, always retuning the new state of the agent.

  This implementation is given by instances defined in this module, which
  delegate work to the custom instances for RunAction defined for the `agent`
  type.
-}
type AgentRunsGenerically :: Type -> (Type -> Type) -> Type -> Constraint

type AgentRunsGenerically action m agent =
  ( GV.Visit action (AsGV agent)
  , Monad m
  , m ~ AgentMonad agent
  )

-- | An AgentOn an action can run that action, returning the Agent's new state
run :: AgentOn action m -> action -> m (AgentOn action m)
run (AgentOn (agent :: agent)) action =
  AgentOn . unAsGV <$> GV.visit (AsGV agent) action

-- | An Agent has an initial state
class
  (Monad (AgentMonad runner)) =>
  Agent runner
  where
  type AgentMonad runner :: Type -> Type
  initial :: AgentMonad runner runner

-- | An agent accepts named actions which transform its state
class
  Agent runner =>
  RunAction (label :: Symbol) action runner
  where
  runAction :: action -> runner -> AgentMonad runner runner

-- | A wrapper that allows an Agent to act as a GenericVisitor
newtype AsGV agent = AsGV {unAsGV :: agent}
  deriving
    (GV.Visitor)
    via (GV.GenericVisitor (AgentMonad agent (AsGV agent)) (AsGV agent))

deriving via
  ( GV.GenericVisitor
      (AgentMonad agent (AsGV agent))
      (AsGV agent)
  )
  instance
    GV.Visit
      entry
      (GV.GenericVisitor (AgentMonad agent (AsGV agent)) (AsGV agent)) =>
    GV.Visit
      entry
      (AsGV agent)

instance
  RunAction label action runner =>
  GV.VisitNamed label action (AsGV runner)
  where
  visitNamed (AsGV r) msg = AsGV <$> runAction @label msg r
  {-# INLINE visitNamed #-}
