{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Utils.Agent.TransformActionName (
  TransformActionName (..),
) where

import Utils.Agent.Class (Agent (..), NewAgent (..), RunAction (..), RunNamedAction (..))
import Utils.TransformSymbol (TransformSymbol)

newtype TransformActionName nameTransform agent = TransformActionName {unTransformActionName :: agent}

deriving newtype instance
  (Monad (AgentMonad agent)) =>
  Agent (TransformActionName nameTransform agent)

instance
  NewAgent agent =>
  NewAgent (TransformActionName nameTransform agent)
  where
  initial = TransformActionName @nameTransform <$> initial

-- deriving newtype (Agent, NewAgent)

instance
  ( transformedName ~ TransformSymbol nameTransform name
  , RunNamedAction transformedName action agent
  ) =>
  RunNamedAction
    name
    action
    (TransformActionName nameTransform agent)
  where
  runNamed action (TransformActionName agent) = TransformActionName <$> runNamed @transformedName action agent
  {-# INLINE runNamed #-}

instance
  ( RunAction action agent
  ) =>
  RunAction action (TransformActionName nameTransform agent)
  where
  run action (TransformActionName agent) = TransformActionName <$> run action agent
  {-# INLINE run #-}
