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
import Data.String (IsString)
import Data.Void (Void)
import Text.Megaparsec qualified as P
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (Project, addResource, addTask, buildProject', projectResources)
import UncertainGantt.Simulator (mostDependentsFirst, simulate)
import UncertainGantt.Task (Task (Task, resource, taskName), TaskName)

runFromFile :: FilePath -> IO ()
runFromFile path = do
  contents <- readFile path
  case P.parse script path contents of
    Left parseError -> throwIO . userError . P.errorBundlePretty $ parseError
    Right script' -> runScript script'

example = Script{scriptProjectDefinition, scriptQueries}
 where
  scriptProjectDefinition =
    ProjectDefinition
      [ ResourceDecl "TeamA" 2
      , ResourceDecl "TeamB" 2
      , TaskDecl "Discovery" "Find things out" "TeamA" medium []
      , TaskDecl "Build A" "Build some stuff" "TeamA" medium ["Discovery"]
      , TaskDecl "Build B" "Build other stuff" "TeamB" small ["Discovery"]
      , TaskDecl "Integrate" "Build other stuff" "TeamB" large ["Build A", "Build B"]
      , TaskDecl "Sell" "Sell stuff" "TeamA" large ["Discovery"]
      , TaskDecl "User Test" "Test" "TeamA" large ["Integrate"]
      ]
  small = UniformD 1 5
  medium = UniformD 6 20
  large = UniformD 21 45
  scriptQueries =
    [ PrintExample
    , PrintExample
    ]

data Script = Script
  { scriptProjectDefinition :: ProjectDefinition
  , scriptQueries :: [Query]
  }

newtype ProjectDefinition = ProjectDefinition
  { projectItems :: [ProjectItem]
  }

data ProjectItem
  = ResourceDecl Resource Word
  | TaskDecl TaskName String Resource DurationD [TaskName]

newtype DurationName = DurantionName String
newtype Resource = Resource String
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

data DurationD
  = UniformD Word Word
  deriving stock (Eq, Ord, Show)

data Query
  = PrintExample

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

-- Parsers

type Parser a = P.Parsec Void String a

script :: Parser Script
script = Script <$> projectDefinition <*> queries

projectDefinition :: Parser ProjectDefinition
projectDefinition = pure $ ProjectDefinition []

queries :: Parser [Query]
queries = pure []
