{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

module UncertainGantt.Project (
  Project (..),
  project,
  addResource,
  addTask,
) where

import Control.Monad.Bayes.Class qualified as Bayes
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import UncertainGantt.Task ( Task(taskName), TaskName )

data Project r d = Project
  { projectTasks :: Map TaskName (Task r d)
  , projectResources :: Map r Word
  , projectDurationEstimator :: forall m. Bayes.MonadSample m => d -> m Word
  }

project :: (forall m. Bayes.MonadSample m => d -> m Word) -> Project r d
project = Project Map.empty Map.empty

addResource :: Ord k => Project k d -> (k, Word) -> Project k d
p `addResource` (resource, amount) = p{projectResources = Map.insert resource amount (projectResources p)}

addTask :: Project r d -> Task r d -> Project r d
p `addTask` task = p{projectTasks = Map.insert (taskName task) task (projectTasks p)}
