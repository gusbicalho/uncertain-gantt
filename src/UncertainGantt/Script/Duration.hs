module UncertainGantt.Script.Duration (
  estimate,
  estimateAverage,
) where

import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler.Strict qualified as Sampler
import UncertainGantt.Script.Stats qualified as Stats
import UncertainGantt.Script.Types (DurationD (..))

{-# SPECIALIZE estimate :: DurationD -> Sampler.SamplerIO Word #-}
{-# SPECIALIZE estimate :: DurationD -> Sampler.SamplerST s Word #-}
estimate :: (Bayes.MonadDistribution m) => DurationD -> m Word
estimate = fmap (max 1) . estimator
 where
  estimator (UniformD from to) = Bayes.uniformD [from .. to]
  estimator (NormalD avg stdDev) =
    round . max 1 <$> Bayes.normal avg stdDev
  -- LogNormalD loosely based on https://erikbern.com/2019/04/15/why-software-projects-take-longer-than-you-think-a-statistical-model.html
  estimator (LogNormalD median logBlowupStdDev) = do
    logBlowup <- Bayes.normal 0 logBlowupStdDev
    pure . round . max 1 $ median * exp logBlowup

{-# SPECIALIZE estimateAverage :: DurationD -> Sampler.SamplerIO Word #-}
{-# SPECIALIZE estimateAverage :: DurationD -> Sampler.SamplerST s Word #-}
estimateAverage :: (Bayes.MonadDistribution m) => DurationD -> m Word
estimateAverage (UniformD from to) = pure $ (from + to) `div` 2
estimateAverage (NormalD avg _) = pure $ round avg
estimateAverage otherDistribution = do
  estimates <- Population.explicitPopulation $ do
    Population.spawn 10000
    fromIntegral <$> estimate otherDistribution
  pure $ case Stats.toSamples estimates of
    Nothing -> error "We just run 10000 estimates, there's no way they are empty"
    Just samples -> round . Stats.weightedAverage $ samples
