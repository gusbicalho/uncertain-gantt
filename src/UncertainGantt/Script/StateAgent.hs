{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module UncertainGantt.Script.StateAgent (
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
import Utils.Agent.Class (Agent (..), NewAgent (..), RunNamedAction (..))

type AnnotatedDurationD = (Maybe String, DurationD)

data StateAgent = StateAgent
  { stateProject :: Project Resource AnnotatedDurationD
  , stateDurationAliases :: Map String DurationD
  , stateSimulations :: Maybe Stats.Samples
  }

instance Agent StateAgent where
  type AgentMonad StateAgent = IO

instance NewAgent StateAgent where
  initial =
    buildProject' (Duration.estimate . snd) (pure ()) <&> \project ->
      StateAgent
        { stateProject = project
        , stateSimulations = Nothing
        , stateDurationAliases = Map.empty
        }

instance RunNamedAction "runAddResource" ResourceDescription StateAgent where
  runNamed (ResourceDescription resource amount) =
    updateProject $ addResource resource amount

instance RunNamedAction "runAddTask" TaskDescription StateAgent where
  runNamed (TaskDescription taskName description resource durationDescription dependencies) state = do
    duration <- resolveDuration state durationDescription
    let action = addTask $ Task taskName description resource duration (Set.fromList dependencies)
    updateProject action state

instance RunNamedAction "runDurationDeclaration" (Maybe String, DurationD) StateAgent where
  runNamed (mbAlias, duration) state = do
    case mbAlias of
      Nothing -> pure state
      Just alias ->
        pure $ state{stateDurationAliases = Map.insert alias duration (stateDurationAliases state)}

instance RunNamedAction "runRunSimulations" Word StateAgent where
  runNamed n state = do
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
