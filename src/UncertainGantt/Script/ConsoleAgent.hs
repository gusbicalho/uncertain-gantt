{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UncertainGantt.Script.ConsoleAgent (
  consoleScriptAgent,
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
  Statement,
  TaskDescription (..),
  unResource,
 )
import UncertainGantt.Simulator qualified as Sim
import UncertainGantt.Task (Task (..), unTaskName)
import Utils.Agent.Generic (Agent (..))
import Utils.Agent.Generic qualified as Agent

type AnnotatedDurationD = (Maybe String, DurationD)

consoleScriptAgent :: IO (Agent.AgentOn Statement IO)
consoleScriptAgent = Agent.AgentOn <$> (initial @ScriptRunner)

data ScriptRunner = ScriptRunner
  { scriptRunnerProject :: Project Resource AnnotatedDurationD
  , scriptRunnerDurationAliases :: Map String DurationD
  , scriptRunnerSimulations :: Maybe Stats.Samples
  }

instance Agent ScriptRunner where
  type AgentMonad ScriptRunner = IO
  initial =
    buildProject' (estimateDuration . snd) (pure ()) <&> \project ->
      ScriptRunner
        { scriptRunnerProject = project
        , scriptRunnerSimulations = Nothing
        , scriptRunnerDurationAliases = Map.empty
        }

instance Agent.RunAction "AddResource" ResourceDescription ScriptRunner where
  runAction (ResourceDescription resource amount) =
    updateProject $ addResource resource amount

instance Agent.RunAction "AddTask" TaskDescription ScriptRunner where
  runAction (TaskDescription taskName description resource durationDescription dependencies) state = do
    duration <- resolveDuration state durationDescription
    let action = addTask $ Task taskName description resource duration (Set.fromList dependencies)
    updateProject action state

instance Agent.RunAction "DurationDeclaration" (Maybe String, DurationD) ScriptRunner where
  runAction (mbAlias, duration) state = do
    describeDuration
    case mbAlias of
      Nothing -> pure state
      Just alias ->
        pure $ state{scriptRunnerDurationAliases = Map.insert alias duration (scriptRunnerDurationAliases state)}
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

instance Agent.RunAction "PrintExample" () ScriptRunner where
  runAction () = notChangingState $ \ScriptRunner{scriptRunnerProject = project} -> do
    putStrLn "Example run:"
    (gantt, Nothing) <- Sampler.sampleIO $ Sim.simulate Sim.mostDependentsFirst project
    Gantt.printGantt (printGanttOptions project) gantt
    putStrLn ""

instance Agent.RunAction "PrintTasks" Bool ScriptRunner where
  runAction briefly = notChangingState $ \ScriptRunner{scriptRunnerProject = project} -> do
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

instance Agent.RunAction "RunSimulations" Word ScriptRunner where
  runAction n state = do
    let project = scriptRunnerProject state
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
    pure $ state{scriptRunnerSimulations = samples}

instance Agent.RunAction "PrintCompletionTimes" () ScriptRunner where
  runAction () = notChangingState $ \ScriptRunner{scriptRunnerSimulations = simulations} -> do
    putStrLn "Completion times:"
    case simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' -> print . F.toList . Stats.getSamples $ simulations'

instance Agent.RunAction "PrintCompletionTimeMean" () ScriptRunner where
  runAction () = notChangingState $ \ScriptRunner{scriptRunnerSimulations = simulations} -> do
    putStr "Completion time mean: "
    case simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . Stats.weightedAverage
          $ simulations'

instance Agent.RunAction "PrintCompletionTimeQuantile" (Word, Word) ScriptRunner where
  runAction (numerator, denominator) =
    notChangingState $ \ScriptRunner{scriptRunnerSimulations = simulations} -> do
      putStr "Completion time "
      if denominator == 100
        then putStr $ "p" <> show numerator <> ": "
        else putStr $ "quantile " <> show numerator <> "/" <> show denominator <> ": "
      case simulations of
        Nothing -> putStrLn "No simulations available."
        Just simulations' ->
          print . Stats.quantile numerator denominator $ simulations'

instance Agent.RunAction "PrintHistogram" Word ScriptRunner where
  runAction numBuckets = notChangingState $ \ScriptRunner{scriptRunnerSimulations = samples} -> do
    case samples of
      Nothing -> putStrLn "Histogram: No simulations available."
      Just samples' -> printHistogram $ Stats.histogram numBuckets (Stats.p99range samples') samples'

notChangingState :: (ScriptRunner -> IO ()) -> ScriptRunner -> IO ScriptRunner
notChangingState action state = action state $> state

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

updateProject :: BuildProjectM Resource AnnotatedDurationD a -> ScriptRunner -> IO ScriptRunner
updateProject update state = do
  project' <- editProject' (scriptRunnerProject state) update
  pure $
    state
      { scriptRunnerProject = project'
      , scriptRunnerSimulations = Nothing
      }

resolveDuration :: ScriptRunner -> Either String DurationD -> IO (Maybe String, DurationD)
resolveDuration _ (Right duration) = pure (Nothing, duration)
resolveDuration state (Left alias) = do
  case Map.lookup alias (scriptRunnerDurationAliases state) of
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
