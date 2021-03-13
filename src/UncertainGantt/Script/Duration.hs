{-# LANGUAGE ScopedTypeVariables #-}
module UncertainGantt.Script.Duration (
  estimate,
  estimateAverage,
) where

import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Population as Population
import qualified Control.Monad.Bayes.Sampler as Sampler
import qualified UncertainGantt.Script.Stats as Stats
import UncertainGantt.Script.Types (DurationD, DurationAST (..))

{-# SPECIALIZE estimate :: DurationD -> Sampler.SamplerIO Word #-}
{-# SPECIALIZE estimate :: DurationD -> Sampler.SamplerST Word #-}
estimate :: forall m. Bayes.MonadSample m => DurationD -> m Word
estimate = fmap (max 1) . estimator
 where
  estimator :: DurationD -> m Word
  estimator (UniformD from to) = Bayes.uniformD [from .. to]
  estimator (NormalD avg stdDev) =
    round . max 1 <$> Bayes.normal avg stdDev
  -- LogNormalD loosely based on https://erikbern.com/2019/04/15/why-software-projects-take-longer-than-you-think-a-statistical-model.html
  estimator (LogNormalD median logBlowupStdDev) = do
    logBlowup <- Bayes.normal 0 logBlowupStdDev
    pure . round . max 1 $ median * exp logBlowup
  estimator (ExactD a) = pure a
  -- TODO fix MinusD underflow
  estimator (d1 `MinusD` d2) = (-) <$> estimator d1 <*> estimator d2
  estimator (d1 `PlusD` d2) = (+) <$> estimator d1 <*> estimator d2

{-# SPECIALIZE estimateAverage :: DurationD -> Sampler.SamplerIO Word #-}
{-# SPECIALIZE estimateAverage :: DurationD -> Sampler.SamplerST Word #-}
estimateAverage :: forall m. Bayes.MonadSample m => DurationD -> m Word
estimateAverage = fmap (max 1) . estimator
 where
  estimator :: DurationD -> m Word
  estimator (UniformD from to) = pure $ (from + to) `div` 2
  estimator (NormalD avg _) = pure $ round avg
  estimator (ExactD a) = pure a
  estimator (d1 `PlusD` d2) = (+) <$> estimator d1 <*> estimator d2
  estimator otherDistribution = do
    estimates <- Population.explicitPopulation $ do
      Population.spawn 10000
      fromIntegral <$> estimate otherDistribution
    pure $ case Stats.toSamples estimates of
      Nothing -> error "We just run 10000 estimates, there's no way they are empty"
      Just samples -> round . Stats.weightedAverage $ samples
