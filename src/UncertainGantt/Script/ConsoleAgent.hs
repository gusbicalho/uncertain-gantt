{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module UncertainGantt.Script.ConsoleAgent (
  consoleScriptAgent,
) where

import Control.Monad (unless)
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler.Strict qualified as Sampler
import Data.Bifunctor (first)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (Project (projectResources, projectTasks))
import UncertainGantt.Script.Duration qualified as Duration
import UncertainGantt.Script.StateAgent (StateAgent)
import UncertainGantt.Script.StateAgent qualified as StateAgent
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.Types (
  DurationD (LogNormalD, NormalD, UniformD),
  PrintGanttType (Average, Random),
  Resource (..),
  Statement (..),
  unResource,
 )
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (..), unTaskName)
import Utils.Agent qualified as Agent

consoleScriptAgent :: IO (Agent.SomeAgent Statement IO)
consoleScriptAgent =
  Agent.someAgent <$> Agent.initial @ConsoleAgent

newtype ConsoleAgent = ConsoleAgent StateAgent
  deriving newtype (Agent.NewAgent)

instance Agent.Agent ConsoleAgent where
  type AgentMonad ConsoleAgent = IO

instance Agent.RunAction Statement ConsoleAgent where
  run stmt agent = runStatement stmt agent

-- | Runs a statement with direct pattern matching
runStatement :: Statement -> ConsoleAgent -> IO ConsoleAgent
runStatement stmt agent@(ConsoleAgent state) = case stmt of
  AddTask taskDesc ->
    ConsoleAgent <$> StateAgent.handleAddTask taskDesc state
  AddResource resourceDesc ->
    ConsoleAgent <$> StateAgent.handleAddResource resourceDesc state
  DurationAliasDeclaration alias duration ->
    ConsoleAgent <$> StateAgent.handleDurationAliasDeclaration (alias, duration) state
  PrintDuration d -> do
    handlePrintDuration d state
    pure agent
  PrintGantt ganttType -> do
    handlePrintGantt ganttType state
    pure agent
  PrintTasks briefly -> do
    handlePrintTasks briefly state
    pure agent
  RunSimulations n -> do
    putStrLn $ "Running " <> show n <> " simulations..."
    ConsoleAgent <$> StateAgent.handleRunSimulations n state
  PrintCompletionTimes -> do
    handlePrintCompletionTimes state
    pure agent
  PrintCompletionTimeMean -> do
    handlePrintCompletionTimeMean state
    pure agent
  PrintCompletionTimeQuantile numerator denominator -> do
    handlePrintCompletionTimeQuantile (numerator, denominator) state
    pure agent
  PrintHistogram numBuckets -> do
    handlePrintHistogram numBuckets state
    pure agent

-- | Handler functions that don't change state, just produce output
handlePrintDuration :: Either String DurationD -> StateAgent -> IO ()
handlePrintDuration d state = do
  StateAgent.resolveDuration state d >>= describeDuration
 where
  describeDuration (mbAlias, duration) = do
    case mbAlias of
      Nothing -> putStrLn $ "duration " <> showDuration duration
      Just alias -> putStrLn $ "duration alias " <> alias <> " = " <> showDuration duration
    samples <-
      fmap (List.sortOn fst)
        . Sampler.sampleIO
        . Population.explicitPopulation
        . (Population.spawn 10000 *>)
        $ Duration.estimate duration
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

handlePrintGantt :: PrintGanttType -> StateAgent -> IO ()
handlePrintGantt ganttType (StateAgent.stateProject -> project) = do
  putStrLn description
  (gantt, Nothing) <-
    Sampler.sampleIO $
      Sim.simulate
        Sim.mostDependentsFirst
        estimator
        project
  Gantt.printGantt (printGanttOptions project) gantt
  putStrLn ""
 where
  (description, estimator) = case ganttType of
    Random -> ("Random run:", Duration.estimate . snd)
    Average -> ("Average run:", Duration.estimateAverage . snd)

handlePrintTasks :: Bool -> StateAgent -> IO ()
handlePrintTasks briefly (StateAgent.stateProject -> project) = do
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
      putStr $
        ": " <> description
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
      putStrLn $
        "  " <> description
  showAnnotatedDuration (Just alias, _) = alias
  showAnnotatedDuration (_, duration) = showDuration duration

handlePrintCompletionTimes :: StateAgent -> IO ()
handlePrintCompletionTimes (StateAgent.stateSimulations -> simulations) = do
  putStrLn "Completion times:"
  case simulations of
    Nothing -> putStrLn "No simulations available."
    Just simulations' -> print . F.toList . Stats.getSamples $ simulations'

handlePrintCompletionTimeMean :: StateAgent -> IO ()
handlePrintCompletionTimeMean (StateAgent.stateSimulations -> simulations) = do
  putStr "Completion time mean: "
  case simulations of
    Nothing -> putStrLn "No simulations available."
    Just simulations' ->
      print
        . Stats.weightedAverage
        $ simulations'

handlePrintCompletionTimeQuantile :: (Word, Word) -> StateAgent -> IO ()
handlePrintCompletionTimeQuantile (numerator, denominator) (StateAgent.stateSimulations -> simulations) = do
  putStr "Completion time "
  if denominator == 100
    then putStr $ "p" <> show numerator <> ": "
    else putStr $ "quantile " <> show numerator <> "/" <> show denominator <> ": "
  case simulations of
    Nothing -> putStrLn "No simulations available."
    Just simulations' ->
      print . Stats.quantile numerator denominator $ simulations'

handlePrintHistogram :: Word -> StateAgent -> IO ()
handlePrintHistogram numBuckets (StateAgent.stateSimulations -> samples) = do
  case samples of
    Nothing -> putStrLn "Histogram: No simulations available."
    Just samples' -> printHistogram $ Stats.histogram numBuckets (Stats.p99range samples') samples'

showDuration :: DurationD -> String
showDuration (UniformD a b) = "uniform " <> show a <> " " <> show b
showDuration (NormalD a b) = "normal " <> show a <> " " <> show b
showDuration (LogNormalD a b) = "logNormal " <> show a <> " " <> show b

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

printGanttOptions :: Project Resource d -> Gantt.PrintGanttOptions Resource d
printGanttOptions project =
  Gantt.defaultPrintOptions
    { Gantt.sortingBy = compare `on` uncurry sortKey
    , Gantt.resourceName = \(Resource s) -> s
    , Gantt.resourceLegend = Maybe.fromMaybe defaultLegendChar . (`Map.lookup` legend)
    }
 where
  sortKey Task{taskName, resource} Gantt.Period{Gantt.fromInclusive, Gantt.toExclusive} =
    (fromInclusive, resource, toExclusive, taskName)
  defaultLegendChar = '#'
  legendChars = "#*>%"
  legend = Map.fromList $ zip (Map.keys $ projectResources project) (cycle legendChars)
