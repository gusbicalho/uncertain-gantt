{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler qualified as Sampler
import Data.Bifunctor (Bifunctor (first))
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import GHC.Real (Ratio ((:%)))
import UncertainGantt qualified as UG
import Data.Ratio ((%))

main :: IO ()
main = do
  putStrLn "Sample:"
  do
    (gantt, Nothing) <- Sampler.sampleIO $ UG.simulate UG.mostDependentsFirst project
    UG.printGantt
      UG.defaultPrintOptions
        { UG.sortingBy = compare `on` uncurry sortKey
        , UG.resourceLegend = teamChar
        }
      gantt
  population <- Sampler.sampleIO . Population.explicitPopulation . (Population.spawn 1000 *>) $ do
    (gantt, _leftovers) <- UG.simulate UG.mostDependentsFirst project
    pure $ UG.completionTime gantt
  case NonEmpty.nonEmpty $ List.sortOn fst population of
    Nothing -> pure ()
    Just weightedSorted -> printStats weightedSorted
  pure ()
 where
  sortKey UG.Task{UG.taskName, UG.resource} UG.Period{UG.fromInclusive, UG.toExclusive} =
    (fromInclusive, resource, toExclusive, taskName)
  teamChar TeamA = '#'
  teamChar TeamB = '*'
  printStats weightedCompletionTimes = do
    let weightedCompletionTimes' = fmap (first fromIntegral) weightedCompletionTimes
        printPercentile n = putStrLn $ "p" <> show n <> ": " <> show (quantile (n % 100) weightedCompletionTimes')
    printPercentile 5
    printPercentile 10
    printPercentile 25
    printPercentile 50
    printPercentile 75
    printPercentile 90
    printPercentile 95

quantile :: Rational -> NonEmpty (Double, Double) -> Double
quantile (numerator :% denominator) ((v, w :: Double) :| vws) = go v w vws
 where
  targetW = fromIntegral numerator * (w + sum (snd <$> vws)) / fromIntegral denominator
  go v _ [] = v
  go v w ((nextV, nextW) : moreVws)
    | w < targetW = go nextV (w + nextW) moreVws
    | otherwise = v + ((nextV - v) * (targetW - w) / (nextW - w))

project :: UG.Project Resource Duration
project =
  UG.project durationD
    `UG.addResource` (TeamA, 2)
    `UG.addResource` (TeamB, 5)
    `UG.addTask` UG.Task "Discovery" "Find things out" TeamA Medium Set.empty
    `UG.addTask` UG.Task "Build A" "Build some stuff" TeamA Medium (Set.fromList ["Discovery"])
    `UG.addTask` UG.Task "Build B" "Build other stuff" TeamB Small (Set.fromList ["Discovery"])
    `UG.addTask` UG.Task "Integrate" "Build other stuff" TeamB Large (Set.fromList ["Build A", "Build B"])
    `UG.addTask` UG.Task "Sell" "Sell stuff" TeamA Large (Set.fromList ["Discovery"])
    `UG.addTask` UG.Task "User Test" "Test" TeamA Large (Set.fromList ["Integrate"])

data Resource = TeamA | TeamB
  deriving stock (Eq, Ord, Show)
data Duration = Small | Medium | Large
  deriving stock (Eq, Ord, Show)

durationD :: (Bayes.MonadSample m, Num a, Enum a) => Duration -> m a
durationD Small = Bayes.uniformD [1 .. 5]
durationD Medium = Bayes.uniformD [5 .. 20]
durationD Large = Bayes.uniformD [20 .. 45]
