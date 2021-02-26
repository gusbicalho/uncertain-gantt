{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module UncertainGantt.Simulator (simulate, mostDependentsFirst) where

import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.State.Strict qualified as StateT
import Control.Monad.Trans.Class qualified as Trans
import Data.Foldable qualified as F
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord (Down (Down))
import Data.Set (Set)
import Data.Set qualified as Set
import UncertainGantt.Gantt (
  Gantt (..),
  Period (Period, toExclusive),
  emptyGantt,
 )
import UncertainGantt.Project (Project (..), transitiveDependents)
import UncertainGantt.Task (
  Task (Task, dependencies, duration, resource, taskName),
  TaskName,
 )

type Prioritization r d = Project r d -> Set TaskName -> [Task r d]

simulate :: (Bayes.MonadSample m, Ord r, Ord d) => Prioritization r d -> Project r d -> m (Gantt r d, Maybe (Map TaskName (Task r d)))
simulate prioritization proj@Project{projectTasks, projectResources, projectDurationEstimator} =
  flip StateT.evalStateT projectResources -- resources
    . flip StateT.evalStateT projectTasks -- todo
    . flip StateT.evalStateT Set.empty -- done
    . flip StateT.evalStateT emptyGantt -- inProgress
    $ go (0 :: Word) -- time
 where
  go t = do
    processCompletions t
    tasksDone <- liftDone StateT.get
    tasksTodo <- liftTodo StateT.get
    tasksInProgress <- inProgressTasks t
    let doable = doableTasks tasksDone tasksTodo
    if Set.null doable && null tasksInProgress
      then finish
      else do
        pickTasks t doable
        go =<< nextRelevantT (t + 1)
  finish = do
    gantt <- liftGantt StateT.get
    todo <- liftTodo StateT.get
    if Map.null todo
      then pure (gantt, Nothing)
      else pure (gantt, Just todo)
  nextRelevantT minT = do
    Gantt gantt <- liftGantt StateT.get
    case NonEmpty.nonEmpty . filter (minT <=) . fmap toExclusive . Map.elems $ gantt of
      Nothing -> pure minT
      Just futureTaskEnds -> pure $ F.minimum futureTaskEnds
  inProgressTasks t = do
    Gantt gantt <- liftGantt StateT.get
    pure $ fmap fst . filter ((t <) . toExclusive . snd) . Map.toAscList $ gantt
  processCompletions t = do
    Gantt gantt <- liftGantt StateT.get
    let completedNow = fmap fst . filter ((t ==) . toExclusive . snd) . Map.toAscList $ gantt
    F.for_ completedNow $ \task -> do
      recordDone (taskName task)
      releaseResource (resource task)
  prioritize = prioritization proj
  pickTasks t doable = do
    F.for_ (prioritize doable) $ \task ->
      takeResource (resource task) >>= \case
        Nothing -> pure ()
        Just _ -> startTask t task
  liftGantt = id
  liftDone = Trans.lift
  liftTodo = Trans.lift . Trans.lift
  liftResources = Trans.lift . Trans.lift . Trans.lift
  liftSample = Trans.lift . Trans.lift . Trans.lift . Trans.lift
  takeResource resource =
    liftResources $
      StateT.gets (Map.lookup resource) >>= \case
        Just n | n > 0 -> do
          StateT.modify' (Map.update (Just . pred) resource)
          pure (Just resource)
        _ -> pure Nothing
  releaseResource resource = liftResources . StateT.modify' $ Map.update (Just . succ) resource
  recordDone taskName = liftDone $ StateT.modify' (Set.insert taskName)
  startTask t task@Task{taskName, duration} = do
    estimate <- liftSample (projectDurationEstimator duration)
    liftGantt . StateT.modify' $ \(Gantt gantt) -> Gantt (Map.insert task (Period t (t + estimate)) gantt)
    liftTodo . StateT.modify' $ Map.delete taskName

doableTasks :: Set TaskName -> Map TaskName (Task r d) -> Set TaskName
doableTasks tasksDone = Map.foldlWithKey pickDoable Set.empty
 where
  pickDoable acc taskName Task{dependencies}
    | all (`Set.member` tasksDone) dependencies = Set.insert taskName acc
    | otherwise = acc

mostDependentsFirst :: Prioritization r d
mostDependentsFirst p@Project{projectTasks} =
  Maybe.mapMaybe (`Map.lookup` projectTasks)
    . List.sortOn (Down . maybe 0 Set.size . (`Map.lookup` taskDependents))
    . F.toList
 where
  taskDependents = transitiveDependents p
