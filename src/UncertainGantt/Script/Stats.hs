{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module UncertainGantt.Script.Stats (
  Samples,
  toSamples,
  getSamples,
  weightedAverage,
  quantile,
  histogram,
  fullRange,
  p99range,
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
  deriving stock (Eq)

fullRange :: Samples -> (Double, Double)
fullRange = minSample &&& maxSample

p99range :: Samples -> (Double, Double)
p99range = quantile 1 200 &&& quantile 199 200

histogram :: Word -> (Double, Double) -> Samples -> [HistogramEntry]
histogram 0 _ _ = []
histogram n (lowerEndFirst, lowerEndLast) UnsafeSamples{samples} =
  let buckets = normalize $ F.foldl' addToBucket IntMap.empty samples
      maxIndex = maybe 0 fst $ IntMap.lookupMax buckets
   in dropWhile ((0 >=) . entryFraction)
        . fmap (histogramEntry buckets)
        $ [0 .. maxIndex]
 where
  bucketSize = (lowerEndLast - lowerEndFirst) / fromIntegral n
  negativeInfinity = negate (1 / 0)
  histogramEntry buckets bucketIndex =
    case IntMap.lookup bucketIndex buckets of
      Nothing -> HistogramEntry (lowerEndForIndex bucketIndex) 0 0
      Just (weight, fraction) -> HistogramEntry (lowerEndForIndex bucketIndex) weight fraction
  addToBucket buckets (sample, weight) = addWeight buckets (indexForSample sample) weight
  indexForSample sample
    | sample < lowerEndFirst = 0
    | lowerEndLast < sample = fromIntegral n
    | otherwise = 1 + floor ((sample - lowerEndFirst) / bucketSize)
  lowerEndForIndex 0 = negativeInfinity
  lowerEndForIndex bucketIndex = lowerEndFirst + (fromIntegral bucketIndex - 1) * bucketSize
  addWeight buckets index weight = case IntMap.lookup index buckets of
    Nothing -> IntMap.insert index weight buckets
    Just previousWeight -> IntMap.insert index (previousWeight + weight) buckets
  normalize buckets =
    let bucketVals = fmap snd . IntMap.toList $ buckets
        maxVal = maximum bucketVals
        sumVal = sum bucketVals
     in fmap ((/ maxVal) &&& (/ sumVal)) buckets
