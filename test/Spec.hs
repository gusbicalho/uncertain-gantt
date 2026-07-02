{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import System.Exit (exitFailure)
import UncertainGantt.Lang.Types (
  DurationD (LogNormalD, NormalD, UniformD),
  ResourceDescription (ResourceDescription),
  TaskDescription (TaskDescription),
 )
import UncertainGantt.Script.Parser (parseScript)
import UncertainGantt.Script.Render (renderDeclarations, renderStatement)
import UncertainGantt.Script.Types (Statement (AddResource, AddTask, DurationAliasDeclaration))
import UncertainGantt.Sim.Stats qualified as Stats

main :: IO ()
main = do
  putStrLn "Tests for uncertain-gantt"
  exampleRoundTrip
  handWrittenRoundTrip
  histogramBuckets

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
