{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

{- | A tolerant counterpart to 'UncertainGantt.Project.BuildProjectM', in
the spirit of the @Validation@ applicative: instead of stopping at the
first invalid declaration, running the builder reports /every/ issue and
still produces a 'Project' containing everything usable.

'BuildProjectM' must be a monad because it validates each insertion
against the state built so far — which is also why declaration order
matters there. 'TolerantBuild' removes the interleaving: declarations
are only collected (a @Writer@, so 'Applicative' composition is the
whole story, like @Validation@), and 'runTolerantBuild' validates the
complete set at once. Order of declarations no longer matters; the last
declaration wins when a name is declared twice.

A task is /excluded/ from the resulting project — with an issue saying
why — when it references an undeclared resource, depends on an
undeclared task, participates in a dependency cycle, or depends
(transitively) on an excluded task. The resulting 'Project' always
satisfies the usual invariants: no dangling references, no cycles.
-}
module UncertainGantt.Project.Tolerant (
  TolerantBuild,
  addResource,
  addTask,
  runTolerantBuild,
  BuildIssue (..),
) where

import Control.Monad.Writer.Strict qualified as Writer
import Data.Foldable qualified as F
import Data.Graph qualified as Graph
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import UncertainGantt.Project (Project (Project, projectResources, projectTasks))
import UncertainGantt.Task (Task (Task, dependencies, resource, taskName), TaskName)

-- | Everything 'runTolerantBuild' can complain about.
data BuildIssue r
  = -- | Declared more than once; the last declaration wins.
    DuplicateResource r
  | -- | Declared more than once; the last declaration wins.
    DuplicateTask TaskName
  | -- | Excluded: the task uses a resource that is not declared.
    TaskMissingResource TaskName r
  | -- | Excluded: the task depends on names that are not declared.
    TaskMissingDependencies TaskName [TaskName]
  | -- | All member tasks excluded: they form a dependency cycle.
    DependencyCycle [TaskName]
  | -- | Excluded: the task depends on tasks that were excluded.
    TaskDependsOnExcluded TaskName [TaskName]
  deriving stock (Eq, Ord, Show)

data Definitions r d = Definitions [(r, Word)] [Task r d]

instance Semigroup (Definitions r d) where
  Definitions resources1 tasks1 <> Definitions resources2 tasks2 =
    Definitions (resources1 <> resources2) (tasks1 <> tasks2)

instance Monoid (Definitions r d) where
  mempty = Definitions [] []

-- | Collects declarations; all validation happens in 'runTolerantBuild'.
newtype TolerantBuild r d a = TolerantBuild (Writer.Writer (Definitions r d) a)
  deriving newtype (Functor, Applicative, Monad)

addResource :: r -> Word -> TolerantBuild r d ()
addResource resource amount = TolerantBuild $ Writer.tell (Definitions [(resource, amount)] [])

addTask :: Task r d -> TolerantBuild r d ()
addTask task = TolerantBuild $ Writer.tell (Definitions [] [task])

{- | Validate all declarations at once. The project contains every
resource and every task that could be included; the issues explain
everything else. An empty issue list means nothing was rejected.
-}
runTolerantBuild :: (Ord r) => TolerantBuild r d a -> (Project r d, [BuildIssue r])
runTolerantBuild (TolerantBuild builder) = (project, issues)
 where
  (_, Definitions resources tasks) = Writer.runWriter builder
  resourceMap = Map.fromList resources
  taskMap = Map.fromList [(taskName t, t) | t <- tasks]
  declaredNames = Map.keysSet taskMap

  duplicateIssues =
    fmap DuplicateResource (duplicates (fst <$> resources))
      <> fmap DuplicateTask (duplicates (taskName <$> tasks))

  missingResourceIssues =
    [ TaskMissingResource name resource
    | (name, Task{resource}) <- Map.toList taskMap
    , resource `Map.notMember` resourceMap
    ]
  missingDependencyIssues =
    [ TaskMissingDependencies name missing
    | (name, Task{dependencies}) <- Map.toList taskMap
    , let missing = filter (`Set.notMember` declaredNames) (F.toList dependencies)
    , not (null missing)
    ]
  cycles =
    [ List.sort (taskName <$> members)
    | Graph.CyclicSCC members <-
        Graph.stronglyConnComp
          [ (t, name, filter (`Set.member` declaredNames) (F.toList (dependencies t)))
          | (name, t) <- Map.toList taskMap
          ]
    ]

  rootExcluded =
    Set.fromList $
      [name | TaskMissingResource name _ <- missingResourceIssues]
        <> [name | TaskMissingDependencies name _ <- missingDependencyIssues]
        <> concat cycles
  (allExcluded, cascadeIssues) = cascade rootExcluded

  cascade excluded =
    case newlyExcluded of
      [] -> (excluded, [])
      _ ->
        let (excluded', laterIssues) = cascade (excluded <> Set.fromList (fst <$> newlyExcluded))
         in (excluded', fmap (uncurry TaskDependsOnExcluded) newlyExcluded <> laterIssues)
   where
    newlyExcluded =
      [ (name, excludedDeps)
      | (name, Task{dependencies}) <- Map.toList taskMap
      , name `Set.notMember` excluded
      , let excludedDeps = filter (`Set.member` excluded) (F.toList dependencies)
      , not (null excludedDeps)
      ]

  project =
    Project
      { projectTasks = Map.withoutKeys taskMap allExcluded
      , projectResources = resourceMap
      }
  issues =
    duplicateIssues
      <> missingResourceIssues
      <> missingDependencyIssues
      <> fmap DependencyCycle cycles
      <> cascadeIssues

duplicates :: (Ord a) => [a] -> [a]
duplicates = Map.keys . Map.filter (> (1 :: Int)) . Map.fromListWith (+) . fmap (,1)
