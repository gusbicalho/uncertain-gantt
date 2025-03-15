{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module UncertainGantt.Script.InterpreterState (
  InterpreterState,
  new,
  stateProject,
  stateDurationAliases,
  stateSimulations,
  resolveDuration,
  handleAddResource,
  handleAddTask,
  handleDurationAliasDeclaration,
  handleRunSimulations,
) where

import Control.Exception (throwIO)
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler.Strict qualified as Sampler
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (BuildProjectM, Project, addResource, addTask, buildProject', editProject')
import UncertainGantt.Script.Duration qualified as Duration
import UncertainGantt.Script.StatementInterpreter (StatementInterpreter (..))
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.Types (
  DurationD,
  Resource (..),
  ResourceDescription (..),
  Statement (..),
  TaskDescription (..),
 )
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (Task))

type AnnotatedDurationD = (Maybe String, DurationD)

data InterpreterState = InterpreterState
  { stateProject :: Project Resource AnnotatedDurationD
  , stateDurationAliases :: Map String DurationD
  , stateSimulations :: Maybe Stats.Samples
  }

new :: IO InterpreterState
new =
  buildProject' (pure ()) <&> \project ->
    InterpreterState
      { stateProject = project
      , stateSimulations = Nothing
      , stateDurationAliases = Map.empty
      }

instance StatementInterpreter InterpreterState where
  interpretStatement stmt state = case stmt of
    AddTask taskDesc ->
      handleAddTask taskDesc state
    AddResource resourceDesc ->
      handleAddResource resourceDesc state
    DurationAliasDeclaration alias duration ->
      handleDurationAliasDeclaration (alias, duration) state
    RunSimulations n -> do
      handleRunSimulations n state
    _ -> pure state

-- Handler functions that modify state
handleAddResource :: ResourceDescription -> InterpreterState -> IO InterpreterState
handleAddResource (ResourceDescription resource amount) =
  updateProject $ addResource resource amount

handleAddTask :: TaskDescription -> InterpreterState -> IO InterpreterState
handleAddTask (TaskDescription taskName description resource durationDescription dependencies) state = do
  duration <- resolveDuration state durationDescription
  let action = addTask $ Task taskName description resource duration (Set.fromList dependencies)
  updateProject action state

handleDurationAliasDeclaration :: (String, DurationD) -> InterpreterState -> IO InterpreterState
handleDurationAliasDeclaration (alias, duration) state = do
  pure $ state{stateDurationAliases = Map.insert alias duration (stateDurationAliases state)}

handleRunSimulations :: Word -> InterpreterState -> IO InterpreterState
handleRunSimulations n state = do
  let project = stateProject state
  population <-
    Sampler.sampleIO
      . Population.explicitPopulation
      . (Population.spawn (fromIntegral n) *>)
      $ Sim.simulate Sim.mostDependentsFirst (Duration.estimate . snd) project
  let samples =
        Stats.toSamples
          . fmap (first (fromIntegral . Gantt.completionTime . fst))
          . filter (Maybe.isNothing . snd . fst)
          $ population
  pure $ state{stateSimulations = samples}

updateProject :: BuildProjectM Resource AnnotatedDurationD a -> InterpreterState -> IO InterpreterState
updateProject update state = do
  project' <- editProject' (stateProject state) update
  pure $
    state
      { stateProject = project'
      , stateSimulations = Nothing
      }

resolveDuration :: InterpreterState -> Either String DurationD -> IO (Maybe String, DurationD)
resolveDuration _ (Right duration) = pure (Nothing, duration)
resolveDuration state (Left alias) = do
  case Map.lookup alias (stateDurationAliases state) of
    Nothing -> throwIO . userError $ "Unknown duration alias " <> alias
    Just duration -> pure (Just alias, duration)
