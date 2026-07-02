{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Exit (exitFailure)
import UncertainGantt qualified as UG
import UncertainGantt.Lang.Types (
  DurationD (LogNormalD, NormalD, UniformD),
  Resource,
  ResourceDescription (ResourceDescription),
  TaskDescription (TaskDescription),
 )
import UncertainGantt.Project.Tolerant qualified as Tolerant
import UncertainGantt.Script.Parser (parseScript)
import UncertainGantt.Script.Render (renderDeclarations, renderStatement)
import UncertainGantt.Script.Types (Statement (AddResource, AddTask, DurationAliasDeclaration))
import UncertainGantt.Sim.Stats qualified as Stats
import UncertainGantt.Toml (
  ProjectEntry (..),
  ProjectsFile (ProjectsFile),
  decodeProjectsFile,
  emptyProjectEntry,
  encodeProjectsFile,
 )

main :: IO ()
main = do
  putStrLn "Tests for uncertain-gantt"
  exampleRoundTrip
  handWrittenRoundTrip
  histogramBuckets
  tomlRoundTrip
  tomlDefaults
  tolerantBuild

{- | The tolerant builder must report every issue at once and still
produce a project containing everything usable.
-}
tolerantBuild :: IO ()
tolerantBuild = do
  let (project, issues) = Tolerant.runTolerantBuild $ do
        Tolerant.addResource devResource 1
        Tolerant.addResource devResource 2
        Tolerant.addTask (task "Ok" devResource [])
        Tolerant.addTask (task "Downstream" devResource ["Ok"])
        Tolerant.addTask (task "NoResource" "Ghost" [])
        Tolerant.addTask (task "NoDep" devResource ["Nowhere"])
        Tolerant.addTask (task "CycleA" devResource ["CycleB"])
        Tolerant.addTask (task "CycleB" devResource ["CycleA"])
        Tolerant.addTask (task "OnCycle" devResource ["CycleA", "Ok"])
  assertEqual "tolerant included tasks" ["Downstream", "Ok"] (Map.keys (UG.projectTasks project))
  assertEqual "tolerant capacity (last wins)" (Map.fromList [(devResource, 2)]) (UG.projectResources project)
  assertEqual
    "tolerant issues"
    [ Tolerant.DuplicateResource devResource
    , Tolerant.TaskMissingResource "NoResource" "Ghost"
    , Tolerant.TaskMissingDependencies "NoDep" ["Nowhere"]
    , Tolerant.DependencyCycle ["CycleA", "CycleB"]
    , Tolerant.TaskDependsOnExcluded "OnCycle" ["CycleA"]
    ]
    issues
 where
  devResource = "Dev" :: Resource
  task name resource deps =
    UG.Task
      { UG.taskName = name
      , UG.description = ""
      , UG.resource = resource
      , UG.duration = ()
      , UG.dependencies = Set.fromList deps
      }

{- | Encoding a projects file and decoding it back must be the identity,
including multiple projects, metadata and quoting-sensitive names.
-}
tomlRoundTrip :: IO ()
tomlRoundTrip = do
  reparsed <- expectRight "decode encoded projects file" (decodeProjectsFile (encodeProjectsFile file))
  assertEqual "toml round-trip" file reparsed
 where
  file = ProjectsFile [projectA, projectB]
  projectA =
    ProjectEntry
      { entryName = "backend rewrite"
      , entryMeta = Map.fromList [("owner", "gus"), ("status", "draft")]
      , entryResources = [ResourceDescription "Dev Team" 3, ResourceDescription "QA" 1]
      , entryDurations = [("small", UniformD 1 5), ("weird name!", LogNormalD 13.0 0.5)]
      , entryTasks =
          [ TaskDescription "Setup" "Set things up" "Dev Team" (Right (NormalD 10.0 2.0)) []
          , TaskDescription "Build It" "" "Dev Team" (Left "small") ["Setup"]
          , TaskDescription "Ship, maybe" "Ship it!" "QA" (Left "weird name!") ["Setup", "Build It"]
          ]
      }
  projectB = (emptyProjectEntry "tiny"){entryTasks = [TaskDescription "Solo" "" "Dev Team" (Right (UniformD 1 2)) []]}

{- | Optional keys (meta, after, description, even whole sections) may be
omitted and decode to their defaults.
-}
tomlDefaults :: IO ()
tomlDefaults = do
  decoded <- expectRight "decode minimal projects file" (decodeProjectsFile minimalFile)
  assertEqual
    "toml defaults"
    ( ProjectsFile
        [ (emptyProjectEntry "minimal")
            { entryResources = [ResourceDescription "Me" 1]
            , entryTasks = [TaskDescription "Only" "" "Me" (Right (UniformD 1 3)) []]
            }
        , emptyProjectEntry "empty"
        ]
    )
    decoded
 where
  minimalFile =
    Text.unlines
      [ "[[project]]"
      , "name = \"minimal\""
      , "[[project.resource]]"
      , "name = \"Me\""
      , "capacity = 1"
      , "[[project.task]]"
      , "name = \"Only\""
      , "resource = \"Me\""
      , "duration = \"uniform 1 3\""
      , ""
      , "[[project]]"
      , "name = \"empty\""
      ]

{- | Bucket lower ends must partition the requested range, and samples on
the range's upper edge must land in the last bucket, not disappear.
-}
histogramBuckets :: IO ()
histogramBuckets = do
  samples <- expectJust "toSamples" (Stats.toSamples [(v, 1) | v <- [1 .. 11]])
  let entries = Stats.histogram 5 (1, 11) samples
  assertEqual "histogram lower ends" [1, 3, 5, 7, 9] (Stats.entryLowerEnd <$> entries)
  assertEqual "histogram last-bucket fraction" (3 / 11) (Stats.entryFraction (last entries))

{- | Rendering the declarative subset of the example script and parsing it
back must produce the same statements.
-}
exampleRoundTrip :: IO ()
exampleRoundTrip = do
  contents <- readFile "resources/example.ug"
  statements <- expectRight "parse resources/example.ug" (parseScript contents)
  let declarations = filter (Maybe.isJust . renderStatement) statements
  reparsed <- expectRight "reparse rendered example.ug" (parseScript (Text.unpack (renderDeclarations statements)))
  assertEqual "example.ug round-trip" declarations reparsed

{- | Round-trip statements exercising quoting, dependencies and all
distribution shapes.
-}
handWrittenRoundTrip :: IO ()
handWrittenRoundTrip = do
  reparsed <- expectRight "reparse rendered hand-written statements" (parseScript rendered)
  assertEqual "hand-written round-trip" statements reparsed
 where
  rendered = Text.unpack (renderDeclarations statements)
  statements =
    [ AddResource (ResourceDescription "Dev Team" 3)
    , AddResource (ResourceDescription "QA" 1)
    , DurationAliasDeclaration "small" (UniformD 1 5)
    , DurationAliasDeclaration "weird name!" (LogNormalD 13.0 0.5)
    , AddTask (TaskDescription "Setup" "Set things up" "Dev Team" (Right (NormalD 10.0 2.0)) [])
    , AddTask (TaskDescription "Build It" "" "Dev Team" (Left "small") ["Setup"])
    , AddTask (TaskDescription "Check, then ship" "Ship it!" "QA" (Left "weird name!") ["Setup", "Build It"])
    ]

expectJust :: String -> Maybe a -> IO a
expectJust label = \case
  Nothing -> do
    putStrLn $ "FAIL " <> label <> ": Nothing"
    exitFailure
  Just a -> pure a

expectRight :: (Show e) => String -> Either e a -> IO a
expectRight label = \case
  Left err -> do
    putStrLn $ "FAIL " <> label <> ": " <> show err
    exitFailure
  Right a -> pure a

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual = do
  unless (expected == actual) $ do
    putStrLn $ "FAIL " <> label
    putStrLn $ "  expected: " <> show expected
    putStrLn $ "  actual:   " <> show actual
    exitFailure
  putStrLn $ "PASS " <> label
