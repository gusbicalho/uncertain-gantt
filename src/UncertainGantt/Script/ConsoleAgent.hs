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
{-# LANGUAGE TypeOperators #-}
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
import Data.Void (absurd)
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (Project (projectResources, projectTasks))
import UncertainGantt.Script.Duration qualified as Duration
import UncertainGantt.Script.StateAgent (StateAgent)
import UncertainGantt.Script.StateAgent qualified as StateAgent
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.Types (
  DurationAST (DurationAliasRef, ExactD, LogNormalD, MinusD, NormalD, PlusD, UniformD),
  DurationD,
  DurationExpr,
  PrintGanttType (Average, Random),
  Resource (..),
  Statement,
  unResource,
 )
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (..), unTaskName)
import Utils.Agent qualified as Agent
import Utils.Compose ((:$), (:.))
import Utils.QualifiedName qualified as QN
import Utils.TransformSymbol qualified as TS

consoleScriptAgent :: IO (Agent.SomeAgent Statement IO)
consoleScriptAgent =
  Agent.someAgent
    <$> Agent.initial
      @( Agent.GenericAgent (QN.Pick 'QN.ConstructorName)
          :. Agent.TransformActionName (TS.Prepend "run")
          :$ ConsoleAgent
       )

newtype ConsoleAgent = ConsoleAgent StateAgent
  deriving newtype (Agent.Agent, Agent.NewAgent)

instance
  {-# OVERLAPPABLE #-}
  Agent.RunNamedAction label action StateAgent =>
  Agent.RunNamedAction label action ConsoleAgent
  where
  runNamed action (ConsoleAgent state) = ConsoleAgent <$> Agent.runNamed @label action state
  {-# INLINE runNamed #-}

instance Agent.RunNamedAction "runPrintDuration" DurationExpr ConsoleAgent where
  runNamed expr agent@(ConsoleAgent state) = do
    StateAgent.resolveDuration state expr >>= describeDuration
    pure agent
   where
    describeDuration duration = do
      let resolvedDurationAsExpr = absurd @String <$> duration
      case traverse (const Nothing) expr :: Maybe DurationD of
        -- above, we attempt to convert the expr into a DurationD, but this
        -- will fail with Nothing if there are any DurationAliasRef in the structure
        -- that way, we know we have to print the raw expr before the resolved duration
        Nothing -> putStrLn $ "duration " <> showDuration expr <> " = " <> showDuration resolvedDurationAsExpr
        Just _ -> putStrLn $ "duration " <> showDuration resolvedDurationAsExpr
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

instance Agent.RunNamedAction "runPrintGantt" PrintGanttType ConsoleAgent where
  runNamed ganttType = notChangingState $ \(StateAgent.stateProject -> project) -> do
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

instance Agent.RunNamedAction "runPrintTasks" Bool ConsoleAgent where
  runNamed briefly = notChangingState $ \(StateAgent.stateProject -> project) -> do
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
      putStrLn $ "  " <> showDuration (fst duration)
      unless (null dependencies) $ do
        putStr "  depends on "
        putStr . List.intercalate "," . fmap unTaskName . F.toList $ dependencies
        putStrLn ""
      unless (null description) $
        putStrLn $ "  " <> description

instance Agent.RunNamedAction "runRunSimulations" Word ConsoleAgent where
  runNamed n (ConsoleAgent state) = do
    putStrLn $ "Running " <> show n <> " simulations..."
    ConsoleAgent <$> Agent.runNamed @"runRunSimulations" n state

instance Agent.RunNamedAction "runPrintCompletionTimes" () ConsoleAgent where
  runNamed () = notChangingState $ \(StateAgent.stateSimulations -> simulations) -> do
    putStrLn "Completion times:"
    case simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' -> print . F.toList . Stats.getSamples $ simulations'

instance Agent.RunNamedAction "runPrintCompletionTimeMean" () ConsoleAgent where
  runNamed () = notChangingState $ \(StateAgent.stateSimulations -> simulations) -> do
    putStr "Completion time mean: "
    case simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . Stats.weightedAverage
          $ simulations'

instance Agent.RunNamedAction "runPrintCompletionTimeQuantile" (Word, Word) ConsoleAgent where
  runNamed (numerator, denominator) =
    notChangingState $ \(StateAgent.stateSimulations -> simulations) -> do
      putStr "Completion time "
      if denominator == 100
        then putStr $ "p" <> show numerator <> ": "
        else putStr $ "quantile " <> show numerator <> "/" <> show denominator <> ": "
      case simulations of
        Nothing -> putStrLn "No simulations available."
        Just simulations' ->
          print . Stats.quantile numerator denominator $ simulations'

instance Agent.RunNamedAction "runPrintHistogram" Word ConsoleAgent where
  runNamed numBuckets = notChangingState $ \(StateAgent.stateSimulations -> samples) -> do
    case samples of
      Nothing -> putStrLn "Histogram: No simulations available."
      Just samples' -> printHistogram $ Stats.histogram numBuckets (Stats.p99range samples') samples'

notChangingState :: (StateAgent -> IO ()) -> ConsoleAgent -> IO ConsoleAgent
notChangingState action agent@(ConsoleAgent state) = action state $> agent

showDuration :: DurationExpr -> String
showDuration (DurationAliasRef alias) = "alias " <> alias
showDuration expr = go expr
 where
  go (UniformD a b) = "uniform " <> show a <> " " <> show b
  go (NormalD a b) = "normal " <> show a <> " " <> show b
  go (LogNormalD a b) = "logNormal " <> show a <> " " <> show b
  go (ExactD a) = "exactly " <> show a
  go (d1 `MinusD` d2) = "(" <> showDuration d1 <> ") - (" <> showDuration d2 <> ")"
  go (d1 `PlusD` d2) = showDuration d1 <> " + " <> showDuration d2
  go (DurationAliasRef alias) = alias

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
