{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Running the Monte Carlo completion-time estimate.
module Editor.Estimate (
  Report (..),
  defaultRuns,
  runReport,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Editor.Doc (Doc, Element (ElemTask), docProjectIssues)
import UncertainGantt qualified as UG
import UncertainGantt.Sim.Estimate qualified as Estimate
import UncertainGantt.Sim.Stats qualified as Stats

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
      Estimate.completionSamples runs id project >>= \case
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
