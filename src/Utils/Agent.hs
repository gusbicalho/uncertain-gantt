{-# LANGUAGE TypeFamilies #-}

module Utils.Agent (
  module Utils.Agent.Class,
  module Utils.Agent.Some,
) where

import Utils.Agent.Class (
  Agent (..),
  AgentOn,
  NewAgent (..),
  RunAction (..),
 )
import Utils.Agent.Some (SomeAgent (..), someAgent)
