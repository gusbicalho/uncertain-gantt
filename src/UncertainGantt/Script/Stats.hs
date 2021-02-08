{-# LANGUAGE NamedFieldPuns #-}

module UncertainGantt.Script.Stats (
  Samples,
  toSamples,
  getSamples,
  weightedAverage,
  quantile,
  histogram,
  HistogramEntry (..),
) where

import Control.Arrow ((&&&))
import qualified Data.Foldable as F
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)

data Samples = UnsafeSamples
  { samples :: NonEmpty (Double, Double)
  , minSample :: Double
  , maxSample :: Double
  , totalWeight :: Double
  }

getSamples :: Samples -> NonEmpty (Double, Double)
getSamples = samples

toSamples :: [(Double, Double)] -> Maybe Samples
toSamples vws = case nonEmpty $ List.sortOn fst vws of
  Nothing -> Nothing
  Just samples ->
    Just $
      UnsafeSamples
        { samples
        , minSample = minimum $ fst <$> samples
        , maxSample = maximum $ fst <$> samples
        , totalWeight = sum $ snd <$> samples
        }

weightedAverage :: Samples -> Double
weightedAverage UnsafeSamples{samples = (v0, w0) :| vws, totalWeight} =
  weightedTotal / totalWeight
 where
  weightedTotal = v0 * w0 + sum (uncurry (*) <$> vws)

quantile :: Word -> Word -> Samples -> Double
quantile numerator denominator UnsafeSamples{samples = (v0, w0) :| vws} = go v0 w0 vws
 where
  targetW = fromIntegral numerator * (w0 + sum (snd <$> vws)) / fromIntegral denominator
  go v _ [] = v
  go v w ((nextV, nextW) : moreVws)
    | w < targetW = go nextV (w + nextW) moreVws
    | otherwise = v + ((nextV - v) * (targetW - w) / (nextW - w))

data HistogramEntry = HistogramEntry
  { entryLowerEnd :: Double
  , entryWeight :: Double -- normalized so the largest bucket is ~1
  , entryFraction :: Double -- normalized so all buckets sum ~1
  }

histogram :: Word -> Samples -> [HistogramEntry]
histogram 0 _ = []
histogram n UnsafeSamples{samples, minSample, maxSample} =
  let buckets = F.foldl' addToBucket IntMap.empty samples
   in fmap (histogramEntry $ normalize buckets) [0 .. fromIntegral n]
 where
  bucketSize = (maxSample - minSample) / fromIntegral n
  histogramEntry buckets bucketIndex =
    let lowerEnd = minSample + fromIntegral bucketIndex * bucketSize
     in case IntMap.lookup bucketIndex buckets of
          Nothing -> HistogramEntry lowerEnd 0 0
          Just (weight, fraction) -> HistogramEntry lowerEnd weight fraction
  addToBucket buckets (sample, weight) =
    addWeight buckets (floor $ (sample - minSample) / bucketSize) weight
  addWeight buckets index weight = case IntMap.lookup index buckets of
    Nothing -> IntMap.insert index weight buckets
    Just previousWeight -> IntMap.insert index (previousWeight + weight) buckets
  normalize buckets =
    let bucketVals = fmap snd . IntMap.toList $ buckets
        maxVal = maximum bucketVals
        sumVal = sum bucketVals
     in fmap ((/ maxVal) &&& (/ sumVal)) buckets
