{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module UncertainGantt.Script.Runner.Default (
  DefaultRunnerState,
  defaultRunnerIO,
) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler qualified as Sampler
import Data.Bifunctor (first)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Functor (($>), (<&>))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (BuildProjectM, Project (projectResources, projectTasks), addResource, addTask, buildProject', editProject')
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.Types (
  DurationD (LogNormalD, NormalD, UniformD),
  Resource (..),
  ResourceDescription (..),
  TaskDescription (..),
  unResource,
 )
import UncertainGantt.Script.Types qualified as Types
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (..), unTaskName)

defaultRunnerIO :: Types.StatementRunner DefaultRunnerState IO
defaultRunnerIO =
  Types.StatementRunner
    { Types.initialState
    , Types.runAddResource
    , Types.runAddTask
    , Types.runDurationDeclaration
    , Types.runPrintExample
    , Types.runPrintTasks
    , Types.runSimulations
    , Types.runPrintCompletionTimes
    , Types.runPrintCompletionTimeMean
    , Types.runPrintCompletionTimeQuantile
    , Types.runPrintHistogram
    }

type AnnotatedDurationD = (Maybe String, DurationD)

data DefaultRunnerState = RunState
  { runStateProject :: Project Resource AnnotatedDurationD
  , runStateDurationAliases :: Map String DurationD
  , runStateSimulations :: Maybe Stats.Samples
  }

initialState :: IO DefaultRunnerState
initialState =
  buildProject' (estimateDuration . snd) (pure ()) <&> \project ->
    RunState
      { runStateProject = project
      , runStateSimulations = Nothing
      , runStateDurationAliases = Map.empty
      }

runAddResource :: ResourceDescription -> DefaultRunnerState -> IO DefaultRunnerState
runAddResource (ResourceDescription resource amount) =
  updateProject $ addResource resource amount

runAddTask :: TaskDescription -> DefaultRunnerState -> IO DefaultRunnerState
runAddTask (TaskDescription taskName description resource durationDescription dependencies) state = do
  duration <- resolveDuration state durationDescription
  let action = addTask $ Task taskName description resource duration (Set.fromList dependencies)
  updateProject action state

runDurationDeclaration :: AnnotatedDurationD -> DefaultRunnerState -> IO DefaultRunnerState
runDurationDeclaration (mbAlias, duration) state = do
  describeDuration
  case mbAlias of
    Nothing -> pure state
    Just alias ->
      pure $ state{runStateDurationAliases = Map.insert alias duration (runStateDurationAliases state)}
 where
  describeDuration = do
    case mbAlias of
      Nothing -> putStrLn $ "duration " <> showDuration duration
      Just alias -> putStrLn $ "duration alias " <> alias <> " = " <> showDuration duration
    samples <-
      fmap (List.sortOn fst)
        . Sampler.sampleIO
        . Population.explicitPopulation
        . (Population.spawn 10000 *>)
        $ estimateDuration duration
    case Stats.toSamples $ first fromIntegral <$> samples of
      Nothing -> pure ()
      Just samples' -> do
        tab *> printMean samples'
        tab *> printPercentile samples' 5
        tab *> printPercentile samples' 10
        tab *> printPercentile samples' 25
        tab *> printPercentile samples' 50
        tab *> printPercentile samples' 75
        tab *> printPercentile samples' 90
        tab *> printPercentile samples' 95
        printHistogram $ Stats.histogram 20 (Stats.p99range samples') samples'
        putStrLn ""
  tab = putStr "  "
  printMean samples = do
    putStrLn $ "Mean: " <> show (Stats.weightedAverage samples)
  printPercentile samples p = do
    putStrLn $ "p" <> show p <> ": " <> show (Stats.quantile p 100 samples)

runPrintExample :: DefaultRunnerState -> IO DefaultRunnerState
runPrintExample = notChangingState $ \RunState{runStateProject = project} -> do
  putStrLn "Example run:"
  (gantt, Nothing) <- Sampler.sampleIO $ Sim.simulate Sim.mostDependentsFirst project
  Gantt.printGantt (printGanttOptions project) gantt
  putStrLn ""

runPrintTasks :: Bool -> DefaultRunnerState -> IO DefaultRunnerState
runPrintTasks briefly = notChangingState $ \RunState{runStateProject = project} -> do
  putStrLn "Tasks:"
  F.traverse_ (printTask briefly)
    . List.sortOn taskName
    . Map.elems
    . projectTasks
    $ project
  putStrLn ""
 where
  printTask True Task{taskName, description} = do
    putStr $ unTaskName taskName
    unless (null description) $
      putStr $ ": " <> description
    putStrLn ""
  printTask False Task{taskName, description, resource, duration, dependencies} = do
    putStrLn $ "task " <> unTaskName taskName
    putStrLn $ "  " <> unResource resource
    putStrLn $ "  " <> showAnnotatedDuration duration
    unless (null dependencies) $ do
      putStr "  depends on "
      putStr . List.intercalate "," . fmap unTaskName . F.toList $ dependencies
      putStrLn ""
    unless (null description) $
      putStrLn $ "  " <> description
  showAnnotatedDuration (Just alias, _) = alias
  showAnnotatedDuration (_, duration) = showDuration duration

showDuration :: DurationD -> String
showDuration (UniformD a b) = "uniform " <> show a <> " " <> show b
showDuration (NormalD a b) = "normal " <> show a <> " " <> show b
showDuration (LogNormalD a b) = "logNormal " <> show a <> " " <> show b

runSimulations :: (Show a, Integral a) => a -> DefaultRunnerState -> IO DefaultRunnerState
runSimulations n state = do
  let project = runStateProject state
  putStrLn $ "Running " <> show n <> " simulations..."
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
  pure $ state{runStateSimulations = samples}

runPrintCompletionTimes :: DefaultRunnerState -> IO DefaultRunnerState
runPrintCompletionTimes = notChangingState $ \RunState{runStateSimulations = simulations} -> do
  putStrLn "Completion times:"
  case simulations of
    Nothing -> putStrLn "No simulations available."
    Just simulations' -> print . F.toList . Stats.getSamples $ simulations'

runPrintCompletionTimeMean :: DefaultRunnerState -> IO DefaultRunnerState
runPrintCompletionTimeMean = notChangingState $ \RunState{runStateSimulations = simulations} -> do
  putStr "Completion time mean: "
  case simulations of
    Nothing -> putStrLn "No simulations available."
    Just simulations' ->
      print
        . Stats.weightedAverage
        $ simulations'

runPrintCompletionTimeQuantile :: (Word, Word) -> DefaultRunnerState -> IO DefaultRunnerState
runPrintCompletionTimeQuantile (numerator, denominator) =
  notChangingState $ \RunState{runStateSimulations = simulations} -> do
    putStr "Completion time "
    if denominator == 100
      then putStr $ "p" <> show numerator <> ": "
      else putStr $ "quantile " <> show numerator <> "/" <> show denominator <> ": "
    case simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print . Stats.quantile numerator denominator $ simulations'

runPrintHistogram :: Word -> DefaultRunnerState -> IO DefaultRunnerState
runPrintHistogram numBuckets = notChangingState $ \RunState{runStateSimulations = samples} -> do
  case samples of
    Nothing -> putStrLn "Histogram: No simulations available."
    Just samples' -> printHistogram $ Stats.histogram numBuckets (Stats.p99range samples') samples'

printHistogram :: [Stats.HistogramEntry] -> IO ()
printHistogram = F.traverse_ printEntry
 where
  printEntry Stats.HistogramEntry{Stats.entryLowerEnd, Stats.entryFraction} =
    putStrLn $ showLowerBound entryLowerEnd <> "  " <> showBar entryFraction <> "  " <> showPercentage entryFraction
  showPercentage n =
    let per10000 = show (round $ n * 10000 :: Integer)
        reversed = reverse per10000
     in reverse $
          "%"
            <> take 2 reversed
            <> "."
            <> (case drop 2 reversed of [] -> "0"; s -> s)
  showLowerBound n = toWidth 8 $ show n
  showBar w = replicate (round $ w * 100) '#'
  toWidth w s =
    case w - length s of
      d
        | d < 0 -> take w s
        | otherwise -> replicate d ' ' <> s

notChangingState :: (DefaultRunnerState -> IO ()) -> DefaultRunnerState -> IO DefaultRunnerState
notChangingState action state = action state $> state

estimateDuration :: Bayes.MonadSample m => DurationD -> m Word
estimateDuration = fmap (max 1) . estimator
 where
  estimator (UniformD from to) = Bayes.uniformD [from .. to]
  estimator (NormalD avg stdDev) =
    round . max 1 <$> Bayes.normal avg stdDev
  -- LogNormalD loosely based on https://erikbern.com/2019/04/15/why-software-projects-take-longer-than-you-think-a-statistical-model.html
  estimator (LogNormalD median logBlowupStdDev) = do
    logBlowup <- Bayes.normal 0 logBlowupStdDev
    pure . round . max 1 $ median * exp logBlowup

updateProject :: BuildProjectM Resource AnnotatedDurationD a -> DefaultRunnerState -> IO DefaultRunnerState
updateProject update state = do
  project' <- editProject' (runStateProject state) update
  pure $
    state
      { runStateProject = project'
      , runStateSimulations = Nothing
      }

resolveDuration :: DefaultRunnerState -> Either String DurationD -> IO (Maybe String, DurationD)
resolveDuration _ (Right duration) = pure (Nothing, duration)
resolveDuration state (Left alias) = do
  case Map.lookup alias (runStateDurationAliases state) of
    Nothing -> throwIO . userError $ "Unknown duration alias " <> alias
    Just duration -> pure (Just alias, duration)

printGanttOptions :: Project Resource d -> Gantt.PrintGanttOptions Resource d
printGanttOptions project =
  Gantt.defaultPrintOptions
    { Gantt.sortingBy = compare `on` uncurry sortKey
    , Gantt.resourceName = \(Resource s) -> s
    , Gantt.resourceLegend = Maybe.fromMaybe (head legendChars) . (`Map.lookup` legend)
    }
 where
  sortKey Task{taskName, resource} Gantt.Period{Gantt.fromInclusive, Gantt.toExclusive} =
    (fromInclusive, resource, toExclusive, taskName)
  legendChars = "#*>%"
  legend = Map.fromList $ zip (Map.keys $ projectResources project) (cycle legendChars)
