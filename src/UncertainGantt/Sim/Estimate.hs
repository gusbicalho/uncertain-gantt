{-# LANGUAGE ImportQualifiedPost #-}

-- | Monte Carlo estimation of project completion times.
module UncertainGantt.Sim.Estimate (
  completionSamples,
) where

import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler.Strict qualified as Sampler
import Data.Bifunctor (first)
import Data.Maybe qualified as Maybe
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Lang.Types (DurationD)
import UncertainGantt.Project (Project)
import UncertainGantt.Sim.Duration qualified as Duration
import UncertainGantt.Sim.Stats qualified as Stats
import UncertainGantt.Simulator qualified as Sim

{- | Simulate the project @n@ times and collect weighted samples of the
completion times of the runs that managed to schedule every task.
'Nothing' when no run completed (or @n@ is 0).
-}
completionSamples ::
  (Ord r, Ord d) =>
  Word ->
  (d -> DurationD) ->
  Project r d ->
  IO (Maybe Stats.Samples)
completionSamples n duration project = do
  population <-
    Sampler.sampleIO
      . Population.explicitPopulation
      . (Population.spawn (fromIntegral n) *>)
      $ Sim.simulate Sim.mostDependentsFirst (Duration.estimate . duration) project
  pure
    . Stats.toSamples
    . fmap (first (fromIntegral . Gantt.completionTime . fst))
    . filter (Maybe.isNothing . snd . fst)
    $ population
