{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UncertainGantt.Script (
  runScript,
  runString,
  runFromFile,
) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler qualified as Sampler
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable qualified as F
import Data.Function (on)
import Data.IORef qualified as IORef
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (Project (projectTasks), addResource, addTask, buildProject', projectResources)
import UncertainGantt.Script.Parser (
  DurationD (..),
  ProjectDefinition (..),
  ProjectItem (..),
  Query (..),
  Resource (..),
  Script (..),
  parseScript,
 )
import UncertainGantt.Simulator (mostDependentsFirst, simulate)
import UncertainGantt.Task (Task (Task, description, resource, taskName), unTaskName)
import qualified Data.List as List

runFromFile :: FilePath -> IO ()
runFromFile path = runString =<< readFile path

runString :: String -> IO ()
runString scriptText =
  case parseScript scriptText of
    Left parseError -> throwIO . userError $ parseError
    Right script -> runScript script

runScript :: Script -> IO ()
runScript Script{scriptProjectDefinition, scriptQueries} = do
  project <- projectFromDefinition scriptProjectDefinition
  runQueries project scriptQueries
  pure ()

projectFromDefinition :: ProjectDefinition -> IO (Project Resource DurationD)
projectFromDefinition ProjectDefinition{projectItems} =
  buildProject' estimator $ F.traverse_ addItem projectItems
 where
  estimator (UniformD from to) = Bayes.uniformD [from .. to]
  addItem (ResourceDecl resource amount) =
    addResource resource amount
  addItem (TaskDecl name desc resource duration deps) =
    addTask $ Task name desc resource duration (Set.fromList deps)

runQueries :: Project Resource DurationD -> [Query] -> IO ()
runQueries project queries = do
  simulations <- IORef.newIORef []
  F.traverse_ (runQuery simulations) queries
 where
  runQuery _ PrintExample = do
    putStrLn "Example run:"
    (gantt, Nothing) <- Sampler.sampleIO $ simulate mostDependentsFirst project
    Gantt.printGantt (printGanttOptions project) gantt
    putStrLn ""
  runQuery _ PrintDescriptions = do
    putStrLn "Tasks:"
    F.for_ (List.sortOn taskName . Map.elems . projectTasks $ project) $ \Task{taskName, description} -> do
      putStr $ unTaskName taskName
      unless (null description) $
        putStr $ ": " <> description
      putStrLn ""
    putStrLn ""
  runQuery simulations_ (RunSimulations n) = do
    putStrLn $ "Running " <> show n <> " simulations..."
    population <-
      Sampler.sampleIO
        . Population.explicitPopulation
        . (Population.spawn 1000 *>)
        $ simulate mostDependentsFirst project
    IORef.writeIORef simulations_ $
      fmap (first fst) . filter (Maybe.isNothing . snd . fst) $ population
  runQuery simulations_ (PrintQuantile numerator denominator) = do
    simulations <- IORef.readIORef simulations_
    if denominator == 100
      then putStr $ "p" <> show numerator <> ": "
      else putStr $ "quantile " <> show numerator <> "/" <> show denominator <> ": "
    case nonEmpty simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . quantile numerator denominator
          . NonEmpty.sortWith fst
          . fmap (first (fromIntegral . Gantt.completionTime))
          $ simulations'

quantile :: Word -> Word -> NonEmpty (Double, Double) -> Double
quantile numerator denominator ((v0, w0) :| vws) = go v0 w0 vws
 where
  targetW = fromIntegral numerator * (w0 + sum (snd <$> vws)) / fromIntegral denominator
  go v _ [] = v
  go v w ((nextV, nextW) : moreVws)
    | w < targetW = go nextV (w + nextW) moreVws
    | otherwise = v + ((nextV - v) * (targetW - w) / (nextW - w))

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
