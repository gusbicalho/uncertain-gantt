{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module UncertainGantt.Project (
  Project (..),
  buildProject,
  editProject,
  addResource,
  addTask,
  BuildProjectM,
  BuildProjectError (..),
  transitiveDependents,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.Except (Except)
import Control.Monad.Except qualified as ExceptT
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as StateT
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map.Lazy qualified as Map.Lazy
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import UncertainGantt.Task (Task (..), TaskName)

data Project r d = Project
  { projectTasks :: Map TaskName (Task r d)
  , projectResources :: Map r Word
  , projectDurationEstimator :: forall m. Bayes.MonadSample m => d -> m Word
  }

data BuildProjectError
  = DependencyCycle TaskName [TaskName]
  | MissingDependencies TaskName [TaskName]
  deriving stock (Eq, Ord, Show)

newtype BuildProjectM r d a = BuildProjectM {runBuildProjectM :: StateT (Project r d) (Except BuildProjectError) a}
  deriving newtype (Functor, Applicative, Monad)

instance Exception BuildProjectError

emptyProject :: (forall m. Bayes.MonadSample m => d -> m Word) -> Project r d
emptyProject = Project Map.empty Map.empty

editProject ::
  Project r d ->
  BuildProjectM r d a ->
  Either BuildProjectError (Project r d)
editProject project =
  ExceptT.runExcept
    . flip StateT.execStateT project
    . runBuildProjectM

editProject' :: Project r d -> BuildProjectM r d a -> IO (Project r d)
editProject' project =
  either throwIt pure . editProject project
 where
  throwIt e = throwIO e

buildProject ::
  (forall m. Bayes.MonadSample m => d -> m Word) ->
  BuildProjectM r d a ->
  Either BuildProjectError (Project r d)
buildProject estimator = editProject (emptyProject estimator)

addResource :: Ord r => r -> Word -> BuildProjectM r d ()
addResource resource amount =
  BuildProjectM . StateT.modify' $ \p ->
    p{projectResources = Map.insert resource amount (projectResources p)}

addTask :: Task r d -> BuildProjectM r d ()
addTask task = BuildProjectM $ do
  project <- StateT.get
  checkMissingDependencies project task
  checkDependencyCycle project task
  StateT.put project{projectTasks = Map.insert (taskName task) task (projectTasks project)}
  pure ()
 where
  checkMissingDependencies project Task{taskName, dependencies} = do
    let missingDeps = filter (`isMissingFrom` project) $ F.toList dependencies
    unless (null missingDeps) $ do
      ExceptT.throwError $ MissingDependencies taskName missingDeps
  isMissingFrom taskName = Map.notMember taskName . projectTasks
  checkDependencyCycle project Task{taskName, dependencies} =
    when (taskName `alreadyExistsIn` project) $ do
      let transitives = transitiveDependents project
          t1 `dependsTransitivelyOn` t2 = case Map.lookup t1 transitives of
            Nothing -> False
            Just deps -> t2 `Set.member` deps
          cycles = filter (`dependsTransitivelyOn` taskName) . F.toList $ dependencies
      unless (null cycles) $
        ExceptT.throwError $ DependencyCycle taskName cycles
  alreadyExistsIn taskName = Map.member taskName . projectTasks

transitiveDependents :: Project r d -> Map.Map TaskName (Set TaskName)
transitiveDependents p = Map.fromList . Map.Lazy.toList $ transitives
 where
  tasks = projectTasks p
  directs =
    Map.Lazy.fromList $
      Map.elems tasks <&> \task ->
        ( taskName task
        , Set.fromList
            . fmap taskName
            . filter ((taskName task `Set.member`) . dependencies)
            . Map.elems
            $ tasks
        )
  transitives =
    flip Map.Lazy.map directs $ \dependents ->
      Set.unions
        . (dependents :)
        . Maybe.mapMaybe (`Map.lookup` transitives)
        . F.toList
        $ dependents
