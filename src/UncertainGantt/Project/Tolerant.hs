{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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

Durations may be declared by alias (@da@) and referenced from tasks
('Either' an alias or a direct value @dd@); 'runTolerantBuild' resolves
the references, so the resulting project carries plain @dd@ durations.

A task is /excluded/ from the resulting project — with an issue saying
why — when it references an undeclared resource or duration alias,
depends on an undeclared task, participates in a dependency cycle, or
depends (transitively) on an excluded task. The resulting 'Project'
always satisfies the usual invariants: no dangling references, no
cycles.
-}
module UncertainGantt.Project.Tolerant (
  TolerantBuild,
  addResource,
  addDurationAlias,
  addTask,
  runTolerantBuild,
  BuildIssue (..),
  issueTasks,
) where

import Control.Monad.Writer.Strict qualified as Writer
import Data.Foldable qualified as F
import Data.Graph qualified as Graph
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import UncertainGantt.Project (Project (Project, projectResources, projectTasks))
import UncertainGantt.Task (Task (Task, dependencies, duration, resource, taskName), TaskName)

-- | Everything 'runTolerantBuild' can complain about.
data BuildIssue r da
  = -- | Declared more than once; the last declaration wins.
    DuplicateResource r
  | -- | Declared more than once; the last declaration wins.
    DuplicateDurationAlias da
  | -- | Declared more than once; the last declaration wins.
    DuplicateTask TaskName
  | -- | Excluded: the task uses a resource that is not declared.
    TaskMissingResource TaskName r
  | -- | Excluded: the task references a duration alias that is not declared.
    TaskUnknownDuration TaskName da
  | -- | Excluded: the task depends on names that are not declared.
    TaskMissingDependencies TaskName [TaskName]
  | -- | All member tasks excluded: they form a dependency cycle.
    DependencyCycle [TaskName]
  | -- | Excluded: the task depends on tasks that were excluded.
    TaskDependsOnExcluded TaskName [TaskName]
  deriving stock (Eq, Ord, Show)

-- | The tasks an issue implicates, for flagging rows in views.
issueTasks :: BuildIssue r da -> [TaskName]
issueTasks = \case
  DuplicateResource _ -> []
  DuplicateDurationAlias _ -> []
  DuplicateTask t -> [t]
  TaskMissingResource t _ -> [t]
  TaskUnknownDuration t _ -> [t]
  TaskMissingDependencies t _ -> [t]
  DependencyCycle ts -> ts
  TaskDependsOnExcluded t _ -> [t]

data Definitions r da dd
  = Definitions [(r, Word)] [(da, dd)] [Task r (Either da dd)]

instance Semigroup (Definitions r da dd) where
  Definitions resources1 aliases1 tasks1 <> Definitions resources2 aliases2 tasks2 =
    Definitions (resources1 <> resources2) (aliases1 <> aliases2) (tasks1 <> tasks2)

instance Monoid (Definitions r da dd) where
  mempty = Definitions [] [] []

-- | Collects declarations; all validation happens in 'runTolerantBuild'.
newtype TolerantBuild r da dd a = TolerantBuild (Writer.Writer (Definitions r da dd) a)
  deriving newtype (Functor, Applicative, Monad)

addResource :: r -> Word -> TolerantBuild r da dd ()
addResource resource amount = TolerantBuild $ Writer.tell (Definitions [(resource, amount)] [] [])

addDurationAlias :: da -> dd -> TolerantBuild r da dd ()
addDurationAlias alias definition = TolerantBuild $ Writer.tell (Definitions [] [(alias, definition)] [])

addTask :: Task r (Either da dd) -> TolerantBuild r da dd ()
addTask task = TolerantBuild $ Writer.tell (Definitions [] [] [task])

{- | Validate all declarations at once, resolving duration aliases. The
project contains every resource and every task that could be included;
the issues explain everything else. An empty issue list means nothing
was rejected.
-}
runTolerantBuild ::
  (Ord r, Ord da) =>
  TolerantBuild r da dd a ->
  (Project r dd, [BuildIssue r da])
runTolerantBuild (TolerantBuild builder) = (project, issues)
 where
  (_, Definitions resources aliases tasks) = Writer.runWriter builder
  resourceMap = Map.fromList resources
  aliasMap = Map.fromList aliases
  taskMap = Map.fromList [(taskName t, t) | t <- tasks]
  declaredNames = Map.keysSet taskMap

  duplicateIssues =
    fmap DuplicateResource (duplicates (fst <$> resources))
      <> fmap DuplicateDurationAlias (duplicates (fst <$> aliases))
      <> fmap DuplicateTask (duplicates (taskName <$> tasks))

  missingResourceIssues =
    [ TaskMissingResource name resource
    | (name, Task{resource}) <- Map.toList taskMap
    , resource `Map.notMember` resourceMap
    ]
  unknownDurationIssues =
    [ TaskUnknownDuration name alias
    | (name, Task{duration = Left alias}) <- Map.toList taskMap
    , alias `Map.notMember` aliasMap
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
        <> [name | TaskUnknownDuration name _ <- unknownDurationIssues]
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

  resolveDuration task = case duration task of
    Right definition -> Just task{duration = definition}
    Left alias -> (\definition -> task{duration = definition}) <$> Map.lookup alias aliasMap
  project =
    Project
      { projectTasks = Map.mapMaybe resolveDuration (Map.withoutKeys taskMap allExcluded)
      , projectResources = resourceMap
      }
  issues =
    duplicateIssues
      <> missingResourceIssues
      <> unknownDurationIssues
      <> missingDependencyIssues
      <> fmap DependencyCycle cycles
      <> cascadeIssues

duplicates :: (Ord a) => [a] -> [a]
duplicates = Map.keys . Map.filter (> (1 :: Int)) . Map.fromListWith (+) . fmap (,1)
