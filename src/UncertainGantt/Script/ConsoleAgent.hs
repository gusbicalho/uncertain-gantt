{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module UncertainGantt.Script.ConsoleAgent (
  consoleScriptAgent,
) where

import Control.Monad (unless)
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler qualified as Sampler
import Data.Bifunctor (first)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Functor (($>))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (Project (projectResources, projectTasks))
import UncertainGantt.Script.Duration qualified as Duration
import UncertainGantt.Script.StateAgent (StateAgent, stateProject, stateSimulations)
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.Types (
  DurationD (LogNormalD, NormalD, UniformD),
  Resource (..),
  Statement,
  unResource,
 )
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (..), unTaskName)
import Utils.Agent.Class (Agent (..), RunAction (..))
import Utils.Agent.Generic qualified as A.Generic
import Utils.Agent.Some (SomeAgent (..))
import Utils.TransformSymbol (Prepend)

consoleScriptAgent :: IO (SomeAgent Statement IO)
consoleScriptAgent = SomeAgent (A.Generic.runGeneric @(Prepend "run")) <$> (initial @ConsoleAgent)

newtype ConsoleAgent = ConsoleAgent StateAgent
  deriving newtype (Agent)

instance
  {-# OVERLAPPABLE #-}
  RunAction label action StateAgent =>
  RunAction label action ConsoleAgent
  where
  runAction action (ConsoleAgent state) = ConsoleAgent <$> runAction @label action state

instance RunAction "runDurationDeclaration" (Maybe String, DurationD) ConsoleAgent where
  runAction (mbAlias, duration) (ConsoleAgent state) = do
    describeDuration
    ConsoleAgent <$> runAction @"runDurationDeclaration" (mbAlias, duration) state
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

instance RunAction "runPrintExample" () ConsoleAgent where
  runAction () = notChangingState $ \(stateProject -> project) -> do
    putStrLn "Example run:"
    (gantt, Nothing) <- Sampler.sampleIO $ Sim.simulate Sim.mostDependentsFirst project
    Gantt.printGantt (printGanttOptions project) gantt
    putStrLn ""

instance RunAction "runPrintTasks" Bool ConsoleAgent where
  runAction briefly = notChangingState $ \(stateProject -> project) -> do
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

instance RunAction "runRunSimulations" Word ConsoleAgent where
  runAction n (ConsoleAgent state) = do
    putStrLn $ "Running " <> show n <> " simulations..."
    ConsoleAgent <$> runAction @"runRunSimulations" n state

instance RunAction "runPrintCompletionTimes" () ConsoleAgent where
  runAction () = notChangingState $ \(stateSimulations -> simulations) -> do
    putStrLn "Completion times:"
    case simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' -> print . F.toList . Stats.getSamples $ simulations'

instance RunAction "runPrintCompletionTimeMean" () ConsoleAgent where
  runAction () = notChangingState $ \(stateSimulations -> simulations) -> do
    putStr "Completion time mean: "
    case simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . Stats.weightedAverage
          $ simulations'

instance RunAction "runPrintCompletionTimeQuantile" (Word, Word) ConsoleAgent where
  runAction (numerator, denominator) =
    notChangingState $ \(stateSimulations -> simulations) -> do
      putStr "Completion time "
      if denominator == 100
        then putStr $ "p" <> show numerator <> ": "
        else putStr $ "quantile " <> show numerator <> "/" <> show denominator <> ": "
      case simulations of
        Nothing -> putStrLn "No simulations available."
        Just simulations' ->
          print . Stats.quantile numerator denominator $ simulations'

instance RunAction "runPrintHistogram" Word ConsoleAgent where
  runAction numBuckets = notChangingState $ \(stateSimulations -> samples) -> do
    case samples of
      Nothing -> putStrLn "Histogram: No simulations available."
      Just samples' -> printHistogram $ Stats.histogram numBuckets (Stats.p99range samples') samples'

notChangingState :: (StateAgent -> IO ()) -> ConsoleAgent -> IO ConsoleAgent
notChangingState action runner@(ConsoleAgent state) = action state $> runner

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
    , Gantt.resourceLegend = Maybe.fromMaybe (head legendChars) . (`Map.lookup` legend)
    }
 where
  sortKey Task{taskName, resource} Gantt.Period{Gantt.fromInclusive, Gantt.toExclusive} =
    (fromInclusive, resource, toExclusive, taskName)
  legendChars = "#*>%"
  legend = Map.fromList $ zip (Map.keys $ projectResources project) (cycle legendChars)
