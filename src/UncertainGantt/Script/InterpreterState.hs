{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Exception (Exception (toException), Handler (Handler), SomeException, catches, throwIO)
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler.Strict qualified as Sampler
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Streaming.Prelude qualified as S
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (BuildProjectError, BuildProjectM, Project, addResource, addTask, buildProject', editProject')
import UncertainGantt.Script.Duration qualified as Duration
import UncertainGantt.Script.StatementInterpreter (StatementInterpreter (..))
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.ToText (ToText (toString))
import UncertainGantt.Script.Types (
  DurationAlias,
  DurationD,
  Resource (..),
  ResourceDescription (..),
  Statement (..),
  TaskDescription (..),
 )
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (Task))

type AnnotatedDurationD = (Maybe DurationAlias, DurationD)

data InterpreterState = InterpreterState
  { stateProject :: Project Resource AnnotatedDurationD
  , stateDurationAliases :: Map DurationAlias DurationD
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
  type Output InterpreterState = SomeException
  interpretStmt state stmt = do
    result <- lift $ handle $ dispatch stmt state
    case result of
      Right state' -> pure state'
      Left ex -> do
        S.yield ex
        pure state
   where
    handle action =
      (Right <$> action)
        `catches` [ Handler $ pure . Left . toException @IOError
                  , Handler $ pure . Left . toException @BuildProjectError
                  ]
    dispatch = \case
      AddTask taskDesc ->
        handleAddTask taskDesc
      AddResource resourceDesc ->
        handleAddResource resourceDesc
      DurationAliasDeclaration alias duration ->
        handleDurationAliasDeclaration (alias, duration)
      RunSimulations n ->
        handleRunSimulations n
      _ -> pure

-- Handler functions that modify state
handleAddResource :: ResourceDescription -> InterpreterState -> IO InterpreterState
handleAddResource (ResourceDescription resource amount) =
  updateProject $ addResource resource amount

handleAddTask :: TaskDescription -> InterpreterState -> IO InterpreterState
handleAddTask (TaskDescription taskName description resource durationDescription dependencies) state = do
  duration <- resolveDuration state durationDescription
  let action = addTask $ Task taskName description resource duration (Set.fromList dependencies)
  updateProject action state

handleDurationAliasDeclaration :: (DurationAlias, DurationD) -> InterpreterState -> IO InterpreterState
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

resolveDuration :: InterpreterState -> Either DurationAlias DurationD -> IO (Maybe DurationAlias, DurationD)
resolveDuration _ (Right duration) = pure (Nothing, duration)
resolveDuration state (Left alias) = do
  case Map.lookup alias (stateDurationAliases state) of
    Nothing -> throwIO . userError $ "Unknown duration alias " <> (toString alias)
    Just duration -> pure (Just alias, duration)
