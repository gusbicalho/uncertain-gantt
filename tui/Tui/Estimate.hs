{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Running and rendering the Monte Carlo completion-time estimate.
module Tui.Estimate (
  Report (..),
  defaultRuns,
  runReport,
  renderReport,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric (showFFloat)
import Tui.Doc (Doc, Element (ElemTask), docProjectIssues)
import UncertainGantt qualified as UG
import UncertainGantt.Sim.Estimate qualified as Estimate
import UncertainGantt.Sim.Stats qualified as Stats
import UncertainGantt.ToText (showText)

data Report = Report
  { reportRuns :: Word
  , reportSamples :: Stats.Samples
  , reportTasksIncluded :: Int
  , reportTasksExcluded :: Int
  }

defaultRuns :: Word
defaultRuns = 1000

{- | Estimate the usable subset of the document (see 'docProjectIssues');
the report says how many tasks were excluded.
-}
runReport :: Word -> Doc -> IO (Either Text Report)
runReport runs doc
  | included == 0 = pure (Left "No usable tasks to simulate")
  | otherwise =
      Estimate.completionSamples runs snd project >>= \case
        Nothing -> pure (Left "No simulation run could schedule every task")
        Just samples ->
          pure . Right $
            Report
              { reportRuns = runs
              , reportSamples = samples
              , reportTasksIncluded = included
              , reportTasksExcluded = defined - included
              }
 where
  (project, _issues) = docProjectIssues doc
  included = Map.size (UG.projectTasks project)
  defined = length [() | ElemTask _ <- doc]

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
