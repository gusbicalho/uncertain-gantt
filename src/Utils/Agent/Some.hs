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

module Utils.Agent.Some (SomeAgent (..), someAgent) where

import Data.Kind (Type)
import Utils.Agent.Class (Agent (..), RunAction (..))

someAgent :: (RunAction action agent) => agent -> SomeAgent action (AgentMonad agent)
someAgent = SomeAgent run

data SomeAgent action (m :: Type -> Type) where
  SomeAgent :: (action -> agent -> m agent) -> agent -> SomeAgent action m

instance (Monad m) => Agent (SomeAgent action m) where
  type AgentMonad (SomeAgent action m) = m

instance
  (Monad m) =>
  RunAction action (SomeAgent action m)
  where
  run action (SomeAgent runNamed agent) =
    SomeAgent runNamed <$> runNamed action agent
  {-# INLINE run #-}
