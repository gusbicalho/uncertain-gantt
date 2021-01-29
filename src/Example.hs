{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Example (main) where

import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler qualified as Sampler
import Data.Bifunctor (Bifunctor (first))
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import GHC.Real (Ratio ((:%)), (%))
import UncertainGantt qualified as UG

main :: IO ()
main = do
  do
    putStrLn "Example:"
    (gantt, Nothing) <- Sampler.sampleIO $ UG.simulate UG.mostDependentsFirst project
    UG.printGantt
      UG.defaultPrintOptions
        { UG.sortingBy = compare `on` uncurry sortKey
        , UG.resourceLegend = teamChar
        }
      gantt
    putStrLn ""
  population <-
    Sampler.sampleIO
      . Population.explicitPopulation
      . (Population.spawn 1000 *>)
      $ UG.simulate UG.mostDependentsFirst project
  case nonEmpty . fmap (first fst) . filter (Maybe.isNothing . snd . fst) $ population of
    Nothing -> pure ()
    Just weightedSorted -> printStats weightedSorted
  pure ()
 where
  sortKey UG.Task{UG.taskName, UG.resource} UG.Period{UG.fromInclusive, UG.toExclusive} =
    (fromInclusive, resource, toExclusive, taskName)
  teamChar TeamA = '#'
  teamChar TeamB = '*'
  printStats weightedResults = do
    let weightedCompletionTimes' =
          NonEmpty.sortWith fst $ first (fromIntegral . UG.completionTime) <$> weightedResults
        printPercentile n = putStrLn $ "p" <> show n <> ": " <> show (quantile (n % 100) weightedCompletionTimes')
    putStrLn $ "Completed samples: " <> show (length weightedResults)
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
  either (error . show) id $
    UG.buildProject durationD $ do
      UG.addResource TeamA 2
      UG.addResource TeamB 5
      UG.addTask $ UG.Task "Discovery" "Find things out" TeamA Medium []
      UG.addTask $ UG.Task "Build A" "Build some stuff" TeamA Medium ["Discovery"]
      UG.addTask $ UG.Task "Build B" "Build other stuff" TeamB Small ["Discovery"]
      UG.addTask $ UG.Task "Integrate" "Build other stuff" TeamB Large ["Build A", "Build B"]
      UG.addTask $ UG.Task "Sell" "Sell stuff" TeamA Large ["Discovery"]
      UG.addTask $ UG.Task "User Test" "Test" TeamA Large ["Integrate"]

data Resource = TeamA | TeamB
  deriving stock (Eq, Ord, Show)
data Duration = Small | Medium | Large
  deriving stock (Eq, Ord, Show)

durationD :: (Bayes.MonadSample m, Num a, Enum a) => Duration -> m a
durationD Small = Bayes.uniformD [1 .. 5]
durationD Medium = Bayes.uniformD [5 .. 20]
durationD Large = Bayes.uniformD [20 .. 45]
