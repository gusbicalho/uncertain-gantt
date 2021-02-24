{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UncertainGantt.Script.StateAgent (
  stateAgent,
  StateAgent,
  stateProject,
  stateDurationAliases,
  stateSimulations,
) where

import Control.Exception (throwIO)
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler qualified as Sampler
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (BuildProjectM, Project, addResource, addTask, buildProject', editProject')
import UncertainGantt.Script.Duration qualified as Duration
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.Types (
  DurationD,
  Resource (..),
  ResourceDescription (..),
  TaskDescription (..),
 )
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (Task))
import Utils.Agent.Class (Agent (..), RunAction (..))
import Utils.Agent.Generic qualified as A.Generic
import Utils.Agent.Some (SomeAgent (..))
import Utils.TransformSymbol (Prepend)

stateAgent :: StateAgentAction action => IO (SomeAgent action IO)
stateAgent = SomeAgent (A.Generic.runGeneric @(Prepend "run")) <$> (initial @StateAgent)

type StateAgentAction action = A.Generic.RunsActionGenerically (Prepend "run") action IO StateAgent
type AnnotatedDurationD = (Maybe String, DurationD)

data StateAgent = StateAgent
  { stateProject :: Project Resource AnnotatedDurationD
  , stateDurationAliases :: Map String DurationD
  , stateSimulations :: Maybe Stats.Samples
  }

instance Agent StateAgent where
  type AgentMonad StateAgent = IO
  initial =
    buildProject' (Duration.estimate . snd) (pure ()) <&> \project ->
      StateAgent
        { stateProject = project
        , stateSimulations = Nothing
        , stateDurationAliases = Map.empty
        }

instance RunAction "runAddResource" ResourceDescription StateAgent where
  runAction (ResourceDescription resource amount) =
    updateProject $ addResource resource amount

instance RunAction "runAddTask" TaskDescription StateAgent where
  runAction (TaskDescription taskName description resource durationDescription dependencies) state = do
    duration <- resolveDuration state durationDescription
    let action = addTask $ Task taskName description resource duration (Set.fromList dependencies)
    updateProject action state

instance RunAction "runDurationDeclaration" (Maybe String, DurationD) StateAgent where
  runAction (mbAlias, duration) state = do
    case mbAlias of
      Nothing -> pure state
      Just alias ->
        pure $ state{stateDurationAliases = Map.insert alias duration (stateDurationAliases state)}

instance RunAction "runRunSimulations" Word StateAgent where
  runAction n state = do
    let project = stateProject state
    population <-
      Sampler.sampleIO
        . Population.explicitPopulation
        . (Population.spawn (fromIntegral n) *>)
        $ Sim.simulate Sim.mostDependentsFirst project
    let samples =
          Stats.toSamples
            . fmap (first (fromIntegral . Gantt.completionTime . fst))
            . filter (Maybe.isNothing . snd . fst)
            $ population
    pure $ state{stateSimulations = samples}

updateProject :: BuildProjectM Resource AnnotatedDurationD a -> StateAgent -> IO StateAgent
updateProject update state = do
  project' <- editProject' (stateProject state) update
  pure $
    state
      { stateProject = project'
      , stateSimulations = Nothing
      }

resolveDuration :: StateAgent -> Either String DurationD -> IO (Maybe String, DurationD)
resolveDuration _ (Right duration) = pure (Nothing, duration)
resolveDuration state (Left alias) = do
  case Map.lookup alias (stateDurationAliases state) of
    Nothing -> throwIO . userError $ "Unknown duration alias " <> alias
    Just duration -> pure (Just alias, duration)
