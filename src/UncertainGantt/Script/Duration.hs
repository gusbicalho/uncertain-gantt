module UncertainGantt.Script.Duration (
  estimate,
) where

import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Sampler as Sampler
import UncertainGantt.Script.Types (DurationD (..))

{-# SPECIALIZE estimate :: DurationD -> Sampler.SamplerIO Word #-}
{-# SPECIALIZE estimate :: DurationD -> Sampler.SamplerST Word #-}
estimate :: Bayes.MonadSample m => DurationD -> m Word
estimate = fmap (max 1) . estimator
 where
  estimator (UniformD from to) = Bayes.uniformD [from .. to]
  estimator (NormalD avg stdDev) =
    round . max 1 <$> Bayes.normal avg stdDev
  -- LogNormalD loosely based on https://erikbern.com/2019/04/15/why-software-projects-take-longer-than-you-think-a-statistical-model.html
  estimator (LogNormalD median logBlowupStdDev) = do
    logBlowup <- Bayes.normal 0 logBlowupStdDev
    pure . round . max 1 $ median * exp logBlowup
