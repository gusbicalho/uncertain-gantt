{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Terminal text rendering for a Monte Carlo estimate 'Report'.
module Tui.EstimateRender (
  renderReport,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Editor.Estimate (Report (Report, reportRuns, reportSamples, reportTasksExcluded, reportTasksIncluded))
import Numeric (showFFloat)
import UncertainGantt.Sim.Stats qualified as Stats
import UncertainGantt.ToText (showText)

renderReport :: Report -> Text
renderReport Report{reportRuns, reportSamples, reportTasksIncluded, reportTasksExcluded} =
  Text.unlines $
    [ showText reportRuns
        <> " simulation runs"
        <> if reportTasksExcluded > 0
          then " (" <> showText reportTasksIncluded <> " tasks; " <> showText reportTasksExcluded <> " excluded)"
          else ""
    , ""
    , "Completion time:"
    , "  mean " <> f1 (Stats.weightedAverage reportSamples)
    ]
      <> [ "  p" <> showText p <> "   " <> f1 (Stats.quantile p 100 reportSamples)
         | p <- [5, 25, 50, 75, 90, 95 :: Word]
         ]
      <> [""]
      <> (histogramLine <$> Stats.histogram 10 (Stats.p99range reportSamples) reportSamples)

histogramLine :: Stats.HistogramEntry -> Text
histogramLine Stats.HistogramEntry{Stats.entryLowerEnd, Stats.entryWeight, Stats.entryFraction} =
  padLeft 8 (if isInfinite entryLowerEnd then "below" else f1 entryLowerEnd)
    <> " "
    <> Text.replicate (round (entryWeight * 30)) "#"
    <> " "
    <> f1 (entryFraction * 100)
    <> "%"

f1 :: Double -> Text
f1 x = Text.pack (showFFloat (Just 1) x "")

padLeft :: Int -> Text -> Text
padLeft width t
  | Text.length t >= width = Text.take width t
  | otherwise = Text.replicate (width - Text.length t) " " <> t
