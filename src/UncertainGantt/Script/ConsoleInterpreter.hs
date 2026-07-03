{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Streaming (Stream)
import Streaming.Prelude qualified as S
import UncertainGantt.Gantt (renderGantt)
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Lang.Render (renderDuration)
import UncertainGantt.Lang.Types (
  DurationAlias,
  DurationD,
  Resource (..),
 )
import UncertainGantt.Project (Project (projectResources, projectTasks))
import UncertainGantt.Script.InterpreterState (InterpreterState)
import UncertainGantt.Script.InterpreterState qualified as InterpreterState
import UncertainGantt.Script.StatementInterpreter (StatementInterpreter (..))
import UncertainGantt.Script.Types (
  PrintGanttType (Average, Random),
  Statement (..),
 )
import UncertainGantt.Sim.Duration qualified as Duration
import UncertainGantt.Sim.Stats qualified as Stats
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (..))
import UncertainGantt.ToText (ToText (toText), showText)

new :: IO ConsoleInterpreter
new = ConsoleInterpreter <$> InterpreterState.new

newtype ConsoleInterpreter = ConsoleInterpreter InterpreterState

instance StatementInterpreter ConsoleInterpreter where
  type Output ConsoleInterpreter = Text
  interpretStmt (ConsoleInterpreter state) stmt = do
    before stmt
    state' <- S.map showError $ interpretStmt state stmt
    after stmt state'
    pure (ConsoleInterpreter state')
   where
    showError = showText
    before (RunSimulations n) = do
      S.yield $ "Running " <> showText n <> " simulations..."
    before _ = pure ()
    after = \case
      PrintDuration d -> do
        handlePrintDuration d
      PrintGantt ganttType -> do
        handlePrintGantt ganttType
      PrintTasks briefly -> do
        handlePrintTasks briefly
      PrintCompletionTimes -> do
        handlePrintCompletionTimes
      PrintCompletionTimeMean -> do
        handlePrintCompletionTimeMean
      PrintCompletionTimeQuantile numerator denominator -> do
        handlePrintCompletionTimeQuantile (numerator, denominator)
      PrintHistogram numBuckets -> do
        handlePrintHistogram numBuckets
      _ -> \_ -> pure ()

-- | Handler functions that don't change state, just produce output
handlePrintDuration :: Either DurationAlias DurationD -> InterpreterState -> Stream (S.Of Text) IO ()
handlePrintDuration d state = do
  lift (InterpreterState.resolveDuration state d) >>= describeDuration
 where
  describeDuration (mbAlias, duration) = do
    case mbAlias of
      Nothing -> S.yield $ "duration " <> renderDuration duration
      Just alias -> S.yield $ "duration alias " <> toText alias <> " = " <> renderDuration duration
    samples <-
      fmap (List.sortOn fst)
        . lift
        . Sampler.sampleIO
        . Population.explicitPopulation
        . (Population.spawn 10000 *>)
        $ Duration.estimate duration
    case Stats.toSamples $ first fromIntegral <$> samples of
      Nothing -> pure ()
      Just samples' -> do
        printMean samples'
        printPercentile samples' 5
        printPercentile samples' 10
        printPercentile samples' 25
        printPercentile samples' 50
        printPercentile samples' 75
        printPercentile samples' 90
        printPercentile samples' 95
        printHistogram $ Stats.histogram 20 (Stats.p99range samples') samples'
        S.yield ""
  printMean samples = do
    S.yield $ "  Mean: " <> showText (Stats.weightedAverage samples)
  printPercentile samples p = do
    S.yield $ "  p" <> showText p <> ": " <> showText (Stats.quantile p 100 samples)

handlePrintGantt :: PrintGanttType -> InterpreterState -> Stream (S.Of Text) IO ()
handlePrintGantt ganttType (InterpreterState.stateProject -> project) = do
  S.yield description
  (gantt, unscheduled) <-
    lift . Sampler.sampleIO $
      Sim.simulate
        Sim.mostDependentsFirst
        estimator
        project
  traverse_ S.yield (renderGantt (printGanttOptions project) gantt)
  F.for_ unscheduled $ \todo ->
    S.yield $
      "Could not schedule (blocked on resource capacity): "
        <> Text.intercalate ", " (toText . taskName <$> Map.elems todo)
 where
  (description, estimator) = case ganttType of
    Random -> ("Random run:", Duration.estimate . snd)
    Average -> ("Average run:", Duration.estimateAverage . snd)

handlePrintTasks :: Bool -> InterpreterState -> Stream (S.Of Text) IO ()
handlePrintTasks briefly (InterpreterState.stateProject -> project) = do
  S.yield "Tasks:"
  F.traverse_ (printTask briefly)
    . List.sortOn taskName
    . Map.elems
    . projectTasks
    $ project
  S.yield ""
 where
  printTask True Task{taskName, description} = do
    S.yield . mconcat $
      [ toText $ taskName
      , if (Text.null description)
          then ""
          else ": " <> toText description
      ]
  printTask False Task{taskName, description, resource, duration, dependencies} = do
    S.yield $ "task " <> toText taskName
    S.yield $ "  " <> toText resource
    S.yield $ "  " <> showAnnotatedDuration duration
    S.yield . mconcat $
      [ if (null dependencies)
          then ""
          else "  depends on "
      , Text.concat . List.intersperse "," . fmap toText . F.toList $ dependencies
      ]
    unless (Text.null description) do
      S.yield $ "  " <> description
  showAnnotatedDuration (Just alias, _) = toText alias
  showAnnotatedDuration (_, duration) = renderDuration duration

handlePrintCompletionTimes :: InterpreterState -> Stream (S.Of Text) IO ()
handlePrintCompletionTimes (InterpreterState.stateSimulations -> simulations) = do
  S.yield . mconcat $
    [ "Completion times:"
    , case simulations of
        Nothing -> "No simulations available."
        Just simulations' -> toText . show . F.toList . Stats.getSamples $ simulations'
    ]

handlePrintCompletionTimeMean :: InterpreterState -> Stream (S.Of Text) IO ()
handlePrintCompletionTimeMean (InterpreterState.stateSimulations -> simulations) = do
  S.yield . mconcat $
    [ "Completion time mean: "
    , case simulations of
        Nothing -> "No simulations available."
        Just simulations' -> toText . show . Stats.weightedAverage $ simulations'
    ]

handlePrintCompletionTimeQuantile :: (Word, Word) -> InterpreterState -> Stream (S.Of Text) IO ()
handlePrintCompletionTimeQuantile (numerator, denominator) (InterpreterState.stateSimulations -> simulations) = do
  S.yield . mconcat $
    [ "Completion time "
    , if denominator == 100
        then "p" <> showText numerator <> ": "
        else "quantile " <> showText numerator <> "/" <> showText denominator <> ": "
    , case simulations of
        Nothing -> "No simulations available."
        Just simulations' ->
          toText . show . Stats.quantile numerator denominator $ simulations'
    ]

handlePrintHistogram :: Word -> InterpreterState -> Stream (S.Of Text) IO ()
handlePrintHistogram numBuckets (InterpreterState.stateSimulations -> samples) = do
  case samples of
    Nothing -> S.yield "Histogram: No simulations available."
    Just samples' -> printHistogram $ Stats.histogram numBuckets (Stats.p99range samples') samples'

printHistogram :: [Stats.HistogramEntry] -> Stream (S.Of Text) IO ()
printHistogram = F.traverse_ printEntry
 where
  printEntry Stats.HistogramEntry{Stats.entryLowerEnd, Stats.entryFraction} =
    S.yield $ showLowerBound entryLowerEnd <> "  " <> showBar entryFraction <> "  " <> showPercentage entryFraction
  showPercentage n =
    let per10000 = showText (round $ n * 10000 :: Integer)
        reversed = Text.reverse per10000
     in Text.reverse $
          "%"
            <> Text.take 2 reversed
            <> "."
            <> (case Text.drop 2 reversed of "" -> "0"; s -> s)
  showLowerBound n = toWidth 8 $ if isInfinite n then "below" else showText n
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
