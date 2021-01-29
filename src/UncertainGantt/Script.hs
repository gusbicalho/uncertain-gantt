{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UncertainGantt.Script where

import Control.Exception (throwIO)
import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.Bayes.Sampler qualified as Sampler
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (Project, addResource, addTask, buildProject', projectResources)
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
import UncertainGantt.Task (Task (Task, resource, taskName))

runFromFile :: FilePath -> IO ()
runFromFile path = runString =<< readFile path

runString :: String -> IO ()
runString scriptText =
  case parseScript scriptText of
    Left parseError -> throwIO . userError $ parseError
    Right script -> runScript script

-- Runner

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
runQueries project = F.traverse_ runQuery
 where
  runQuery PrintExample = do
    putStrLn "Example run:"
    (gantt, Nothing) <- Sampler.sampleIO $ simulate mostDependentsFirst project
    Gantt.printGantt (printGanttOptions project) gantt
    putStrLn ""

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
  legendChars = "#*="
  legend = Map.fromList $ zip (Map.keys $ projectResources project) (cycle legendChars)
