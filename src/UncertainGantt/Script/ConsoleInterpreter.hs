{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module UncertainGantt.Script.ConsoleInterpreter (
  new,
  ConsoleInterpreter,
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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (Project (projectResources, projectTasks))
import UncertainGantt.Script.Duration qualified as Duration
import UncertainGantt.Script.InterpreterState (InterpreterState)
import UncertainGantt.Script.InterpreterState qualified as InterpreterState
import UncertainGantt.Script.StatementInterpreter (StatementInterpreter (interpretStatement))
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.ToText (ToText (toText), showText)
import UncertainGantt.Script.Types (
  DurationAlias,
  DurationD (LogNormalD, NormalD, UniformD),
  PrintGanttType (Average, Random),
  Resource (..),
  Statement (..),
 )
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (..))

new :: IO ConsoleInterpreter
new = ConsoleInterpreter <$> InterpreterState.new

newtype ConsoleInterpreter = ConsoleInterpreter InterpreterState

instance StatementInterpreter ConsoleInterpreter where
  interpretStatement stmt (ConsoleInterpreter state) = do
    before
    state <- interpretStatement stmt state
    after state
    pure $ ConsoleInterpreter state
   where
    before = case stmt of
      RunSimulations n -> do
        Text.IO.putStrLn $ "Running " <> showText n <> " simulations..."
      _ -> pure ()
    after state = case stmt of
      PrintDuration d -> do
        handlePrintDuration d state
      PrintGantt ganttType -> do
        handlePrintGantt ganttType state
      PrintTasks briefly -> do
        handlePrintTasks briefly state
      PrintCompletionTimes -> do
        handlePrintCompletionTimes state
      PrintCompletionTimeMean -> do
        handlePrintCompletionTimeMean state
      PrintCompletionTimeQuantile numerator denominator -> do
        handlePrintCompletionTimeQuantile (numerator, denominator) state
      PrintHistogram numBuckets -> do
        handlePrintHistogram numBuckets state
      _ -> pure ()

-- | Handler functions that don't change state, just produce output
handlePrintDuration :: Either DurationAlias DurationD -> InterpreterState -> IO ()
handlePrintDuration d state = do
  InterpreterState.resolveDuration state d >>= describeDuration
 where
  describeDuration (mbAlias, duration) = do
    case mbAlias of
      Nothing -> Text.IO.putStrLn $ "duration " <> showDuration duration
      Just alias -> Text.IO.putStrLn $ "duration alias " <> toText alias <> " = " <> showDuration duration
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
        Text.IO.putStrLn ""
  tab = Text.IO.putStr "  "
  printMean samples = do
    Text.IO.putStrLn $ "Mean: " <> showText (Stats.weightedAverage samples)
  printPercentile samples p = do
    Text.IO.putStrLn $ "p" <> showText p <> ": " <> showText (Stats.quantile p 100 samples)

handlePrintGantt :: PrintGanttType -> InterpreterState -> IO ()
handlePrintGantt ganttType (InterpreterState.stateProject -> project) = do
  Text.IO.putStrLn description
  (gantt, Nothing) <-
    Sampler.sampleIO $
      Sim.simulate
        Sim.mostDependentsFirst
        estimator
        project
  Gantt.printGantt (printGanttOptions project) gantt
  Text.IO.putStrLn ""
 where
  (description, estimator) = case ganttType of
    Random -> ("Random run:", Duration.estimate . snd)
    Average -> ("Average run:", Duration.estimateAverage . snd)

handlePrintTasks :: Bool -> InterpreterState -> IO ()
handlePrintTasks briefly (InterpreterState.stateProject -> project) = do
  Text.IO.putStrLn "Tasks:"
  F.traverse_ (printTask briefly)
    . List.sortOn taskName
    . Map.elems
    . projectTasks
    $ project
  Text.IO.putStrLn ""
 where
  printTask True Task{taskName, description} = do
    Text.IO.putStr . toText $ taskName
    unless (Text.null description) $
      Text.IO.putStr $
        ": " <> toText description
    Text.IO.putStrLn ""
  printTask False Task{taskName, description, resource, duration, dependencies} = do
    Text.IO.putStrLn $ "task " <> toText taskName
    Text.IO.putStrLn $ "  " <> toText resource
    Text.IO.putStrLn $ "  " <> showAnnotatedDuration duration
    unless (null dependencies) $ do
      Text.IO.putStr "  depends on "
      Text.IO.putStr . Text.concat . List.intersperse "," . fmap toText . F.toList $ dependencies
      Text.IO.putStrLn ""
    unless (Text.null description) $
      Text.IO.putStrLn $
        "  " <> description
  showAnnotatedDuration (Just alias, _) = toText alias
  showAnnotatedDuration (_, duration) = showDuration duration

handlePrintCompletionTimes :: InterpreterState -> IO ()
handlePrintCompletionTimes (InterpreterState.stateSimulations -> simulations) = do
  Text.IO.putStrLn "Completion times:"
  case simulations of
    Nothing -> Text.IO.putStrLn "No simulations available."
    Just simulations' -> print . F.toList . Stats.getSamples $ simulations'

handlePrintCompletionTimeMean :: InterpreterState -> IO ()
handlePrintCompletionTimeMean (InterpreterState.stateSimulations -> simulations) = do
  Text.IO.putStr "Completion time mean: "
  case simulations of
    Nothing -> Text.IO.putStrLn "No simulations available."
    Just simulations' ->
      print
        . Stats.weightedAverage
        $ simulations'

handlePrintCompletionTimeQuantile :: (Word, Word) -> InterpreterState -> IO ()
handlePrintCompletionTimeQuantile (numerator, denominator) (InterpreterState.stateSimulations -> simulations) = do
  Text.IO.putStr "Completion time "
  if denominator == 100
    then Text.IO.putStr $ "p" <> showText numerator <> ": "
    else Text.IO.putStr $ "quantile " <> showText numerator <> "/" <> showText denominator <> ": "
  case simulations of
    Nothing -> Text.IO.putStrLn "No simulations available."
    Just simulations' ->
      print . Stats.quantile numerator denominator $ simulations'

handlePrintHistogram :: Word -> InterpreterState -> IO ()
handlePrintHistogram numBuckets (InterpreterState.stateSimulations -> samples) = do
  case samples of
    Nothing -> Text.IO.putStrLn "Histogram: No simulations available."
    Just samples' -> printHistogram $ Stats.histogram numBuckets (Stats.p99range samples') samples'

showDuration :: DurationD -> Text
showDuration (UniformD a b) = "uniform " <> showText a <> " " <> showText b
showDuration (NormalD a b) = "normal " <> showText a <> " " <> showText b
showDuration (LogNormalD a b) = "logNormal " <> showText a <> " " <> showText b

printHistogram :: [Stats.HistogramEntry] -> IO ()
printHistogram = F.traverse_ printEntry
 where
  printEntry Stats.HistogramEntry{Stats.entryLowerEnd, Stats.entryFraction} =
    Text.IO.putStrLn $ showLowerBound entryLowerEnd <> "  " <> showBar entryFraction <> "  " <> showPercentage entryFraction
  showPercentage n =
    let per10000 = showText (round $ n * 10000 :: Integer)
        reversed = Text.reverse per10000
     in Text.reverse $
          "%"
            <> Text.take 2 reversed
            <> "."
            <> (case Text.drop 2 reversed of "" -> "0"; s -> s)
  showLowerBound n = toWidth 8 $ showText n
  showBar w = Text.replicate (round $ w * 100) "#"
  toWidth w s =
    case w - Text.length s of
      d
        | d < 0 -> Text.take w s
        | otherwise -> Text.replicate d " " <> s

printGanttOptions :: Project Resource d -> Gantt.PrintGanttOptions Resource d
printGanttOptions project =
  Gantt.defaultPrintOptions
    { Gantt.sortingBy = compare `on` uncurry sortKey
    , Gantt.resourceName = toText
    , Gantt.resourceLegend = Maybe.fromMaybe defaultLegendChar . (`Map.lookup` legend)
    }
 where
  sortKey Task{taskName, resource} Gantt.Period{Gantt.fromInclusive, Gantt.toExclusive} =
    (fromInclusive, resource, toExclusive, taskName)
  defaultLegendChar = '#'
  legendChars = "#*>%"
  legend = Map.fromList $ zip (Map.keys $ projectResources project) (cycle legendChars)
