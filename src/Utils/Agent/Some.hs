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

module Utils.Agent.Some (SomeAgent (..), run) where

import Data.Kind (Type)

data SomeAgent action (m :: Type -> Type) where
  SomeAgent :: (agent -> action -> m agent) -> agent -> SomeAgent action m

-- | SomeAgent for an action can run that action, returning the Agent's new state
run :: Functor m => SomeAgent action m -> action -> m (SomeAgent action m)
run (SomeAgent runAction agent) action =
  SomeAgent runAction <$> runAction agent action
{-# INLINE run #-}
