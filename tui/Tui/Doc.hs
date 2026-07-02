{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The editable project document: an ordered list of project elements
(resources, duration aliases, tasks) that the TUI manipulates directly.
Scripts are only used as the persistence format ('fromStatements' /
'toStatements').
-}
module Tui.Doc (
  Doc,
  Element (..),
  DocOp (..),
  applyOp,
  fromStatements,
  toStatements,
  docProject,
  DocProject,
  FormSpec (..),
  newResourceSpec,
  newAliasSpec,
  newTaskSpec,
  editSpec,
) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text.Read
import Tui.Widgets (FormField (FormField, fieldCompletions, fieldInitial, fieldLabel))
import UncertainGantt qualified as UG
import UncertainGantt.Script.Parser (parseDurationDescription)
import UncertainGantt.Script.Render (renderDuration)
import UncertainGantt.Script.ToText (ToText (toText), showText)
import UncertainGantt.Script.Types (
  DurationAlias,
  DurationD,
  Resource,
  ResourceDescription (ResourceDescription),
  Statement (AddResource, AddTask, DurationAliasDeclaration),
  TaskDescription (TaskDescription),
  unDurationAlias,
  unResource,
 )

data Element
  = ElemResource ResourceDescription
  | ElemAlias DurationAlias DurationD
  | ElemTask TaskDescription
  deriving stock (Eq, Show)

type Doc = [Element]

data DocOp
  = OpInsert Element
  | OpReplace Int Element
  | OpDelete Int

{- | Apply an edit. 'OpReplace' propagates renames: replacing an element
with one of the same kind under a new name updates every task that
referenced the old name (resource, duration alias or dependency), so a
rename never silently orphans tasks.
-}
applyOp :: DocOp -> Doc -> Doc
applyOp = \case
  OpInsert element -> (<> [element])
  OpReplace i element -> \doc ->
    let replaced = zipWith (\ix old -> if ix == i then element else old) [0 ..] doc
     in case drop i doc of
          (old : _) -> propagateRename old element replaced
          [] -> replaced
  OpDelete i -> fmap snd . filter ((/= i) . fst) . zip [0 ..]

propagateRename :: Element -> Element -> Doc -> Doc
propagateRename old new = case (old, new) of
  (ElemResource (ResourceDescription from _), ElemResource (ResourceDescription to _))
    | from /= to -> mapTasks $ \(TaskDescription name description resource duration deps) ->
        TaskDescription name description (if resource == from then to else resource) duration deps
  (ElemAlias from _, ElemAlias to _)
    | from /= to -> mapTasks $ \(TaskDescription name description resource duration deps) ->
        TaskDescription name description resource (either (Left . replacing from to) Right duration) deps
  (ElemTask oldTask, ElemTask newTask)
    | from <- taskDescName oldTask
    , to <- taskDescName newTask
    , from /= to ->
        mapTasks $ \(TaskDescription name description resource duration deps) ->
          TaskDescription name description resource duration (replacing from to <$> deps)
  _ -> id
 where
  replacing from to x = if x == from then to else x
  mapTasks f = fmap $ \case
    ElemTask t -> ElemTask (f t)
    element -> element

{- | Extract the editable elements; also reports how many statements were
not declarative (prints, runs) and thus dropped.
-}
fromStatements :: [Statement] -> (Doc, Int)
fromStatements statements = (elements, length statements - length elements)
 where
  elements = Maybe.mapMaybe toElement statements
  toElement = \case
    AddResource r -> Just (ElemResource r)
    DurationAliasDeclaration a d -> Just (ElemAlias a d)
    AddTask t -> Just (ElemTask t)
    _ -> Nothing

{- | Statements in an order the sequential script interpreter accepts:
resources and duration aliases first, then tasks sorted so dependencies
come before dependents.
-}
toStatements :: Doc -> [Statement]
toStatements doc =
  fmap AddResource resources
    <> fmap (uncurry DurationAliasDeclaration) aliases
    <> fmap AddTask (fst (sortTasks tasks))
 where
  (resources, aliases, tasks) = partitionDoc doc

partitionDoc :: Doc -> ([ResourceDescription], [(DurationAlias, DurationD)], [TaskDescription])
partitionDoc doc =
  ( [r | ElemResource r <- doc]
  , [(a, d) | ElemAlias a d <- doc]
  , [t | ElemTask t <- doc]
  )

{- | Order tasks so in-document dependencies come first. Dependencies on
names not present in the document are ignored here (project building
reports them). The second component holds tasks stuck in dependency
cycles.
-}
sortTasks :: [TaskDescription] -> ([TaskDescription], [TaskDescription])
sortTasks tasks = go Set.empty tasks
 where
  present = Set.fromList (taskDescName <$> tasks)
  go emitted pending =
    case List.partition ready pending of
      ([], stuck) -> ([], stuck)
      (readyNow, rest) ->
        let emitted' = foldr (Set.insert . taskDescName) emitted readyNow
            (sorted, stuck) = go emitted' rest
         in (readyNow <> sorted, stuck)
   where
    ready t =
      all
        (\dep -> dep `Set.member` emitted || dep `Set.notMember` present)
        (taskDescDeps t)

taskDescName :: TaskDescription -> UG.TaskName
taskDescName (TaskDescription n _ _ _ _) = n

taskDescDeps :: TaskDescription -> [UG.TaskName]
taskDescDeps (TaskDescription _ _ _ _ deps) = deps

type DocProject = UG.Project Resource (Maybe DurationAlias, DurationD)

-- | Validate the document and build the domain-model project from it.
docProject :: Doc -> Either Text DocProject
docProject doc = do
  checkDuplicates "resource" [toText (unResource r) | ResourceDescription r _ <- resources]
  checkDuplicates "duration" [toText (unDurationAlias a) | (a, _) <- aliases]
  checkDuplicates "task" [toText (UG.unTaskName (taskDescName t)) | t <- tasks]
  case stuck of
    [] -> pure ()
    ts ->
      Left $
        "Dependency cycle involving: "
          <> Text.intercalate ", " (toText . UG.unTaskName . taskDescName <$> ts)
  resolved <- traverse resolveTask sorted
  case UG.buildProject (addAll resolved) of
    Left err -> Left (prettyBuildError err)
    Right project -> Right project
 where
  (resources, aliases, tasks) = partitionDoc doc
  (sorted, stuck) = sortTasks tasks
  aliasMap = Map.fromList aliases
  addAll resolved = do
    mapM_ (\(ResourceDescription r amount) -> UG.addResource r amount) resources
    mapM_ UG.addTask resolved
  resolveTask (TaskDescription taskName description resource duration dependencies) = do
    duration' <- case duration of
      Right d -> Right (Nothing, d)
      Left alias -> case Map.lookup alias aliasMap of
        Nothing ->
          Left $
            "Task "
              <> toText (UG.unTaskName taskName)
              <> " uses unknown duration "
              <> toText (unDurationAlias alias)
        Just d -> Right (Just alias, d)
    pure
      UG.Task
        { UG.taskName = taskName
        , UG.description = description
        , UG.resource = resource
        , UG.duration = duration'
        , UG.dependencies = Set.fromList dependencies
        }

checkDuplicates :: Text -> [Text] -> Either Text ()
checkDuplicates kind names =
  case Map.keys . Map.filter (> (1 :: Int)) . Map.fromListWith (+) $ (,1) <$> names of
    [] -> Right ()
    dupes -> Left $ "Duplicate " <> kind <> " names: " <> Text.intercalate ", " dupes

prettyBuildError :: UG.BuildProjectError -> Text
prettyBuildError = \case
  UG.MissingResource t ->
    "Task " <> toText (UG.unTaskName t) <> " uses a resource that is not defined"
  UG.MissingDependencies t deps ->
    "Task "
      <> toText (UG.unTaskName t)
      <> " depends on undefined tasks: "
      <> Text.intercalate ", " (toText . UG.unTaskName <$> deps)
  UG.DependencyCycle t deps ->
    "Dependency cycle: "
      <> toText (UG.unTaskName t)
      <> " <-> "
      <> Text.intercalate ", " (toText . UG.unTaskName <$> deps)

-- * Forms

{- | A pure description of an add/edit form: what to show, and how to turn
the submitted field values back into an 'Element'.
-}
data FormSpec = FormSpec
  { formTitle :: Text
  , formFields :: [FormField]
  , formParse :: [Text] -> Either Text Element
  }

newResourceSpec :: FormSpec
newResourceSpec = resourceSpec Nothing

newAliasSpec :: FormSpec
newAliasSpec = aliasSpec Nothing

newTaskSpec :: Doc -> FormSpec
newTaskSpec doc = taskSpec doc Nothing

-- | Form for editing an existing element, prefilled.
editSpec :: Doc -> Element -> FormSpec
editSpec doc = \case
  ElemResource r -> resourceSpec (Just r)
  ElemAlias a d -> aliasSpec (Just (a, d))
  ElemTask t -> taskSpec doc (Just t)

-- | A form field without completions.
plainField :: Text -> Text -> FormField
plainField label initial =
  FormField{fieldLabel = label, fieldInitial = initial, fieldCompletions = []}

distributionKeywords :: [Text]
distributionKeywords = ["uniform", "normal", "logNormal"]

resourceSpec :: Maybe ResourceDescription -> FormSpec
resourceSpec existing =
  FormSpec
    { formTitle = maybe "Add resource" (const "Edit resource") existing
    , formFields =
        [ plainField "Name" (maybe "" (\(ResourceDescription r _) -> toText (unResource r)) existing)
        , plainField "Capacity" (maybe "" (\(ResourceDescription _ n) -> showText n) existing)
        ]
    , formParse = \case
        [name, capacity] -> do
          name' <- requireName "Name" name
          capacity' <- parseWord "Capacity" capacity
          pure $ ElemResource (ResourceDescription (fromString (Text.unpack name')) capacity')
        _ -> Left "wrong number of fields"
    }

aliasSpec :: Maybe (DurationAlias, DurationD) -> FormSpec
aliasSpec existing =
  FormSpec
    { formTitle = maybe "Add duration alias" (const "Edit duration alias") existing
    , formFields =
        [ plainField "Name" (maybe "" (toText . unDurationAlias . fst) existing)
        , FormField
            { fieldLabel = "Distribution"
            , fieldInitial = maybe "" (renderDuration . snd) existing
            , fieldCompletions = distributionKeywords
            }
        ]
    , formParse = \case
        [name, distribution] -> do
          name' <- requireName "Name" name
          duration <- parseDistribution distribution
          pure $ ElemAlias (fromString (Text.unpack name')) duration
        _ -> Left "wrong number of fields"
    }

taskSpec :: Doc -> Maybe TaskDescription -> FormSpec
taskSpec doc existing =
  FormSpec
    { formTitle = maybe "Add task" (const "Edit task") existing
    , formFields =
        [ plainField "Name" (maybe "" (\(TaskDescription n _ _ _ _) -> toText (UG.unTaskName n)) existing)
        , FormField
            { fieldLabel = "Resource"
            , fieldInitial = maybe "" (\(TaskDescription _ _ r _ _) -> toText (unResource r)) existing
            , fieldCompletions = [toText (unResource r) | ElemResource (ResourceDescription r _) <- doc]
            }
        , FormField
            { fieldLabel = "Duration"
            , fieldInitial =
                maybe
                  ""
                  ( \(TaskDescription _ _ _ d _) ->
                      either (toText . unDurationAlias) renderDuration d
                  )
                  existing
            , fieldCompletions =
                [toText (unDurationAlias a) | ElemAlias a _ <- doc] <> distributionKeywords
            }
        , FormField
            { fieldLabel = "Depends on"
            , fieldInitial =
                maybe
                  ""
                  ( \(TaskDescription _ _ _ _ deps) ->
                      Text.intercalate ", " (toText . UG.unTaskName <$> deps)
                  )
                  existing
            , fieldCompletions =
                [ toText (UG.unTaskName name)
                | ElemTask t <- doc
                , let name = taskDescName t
                , Just name /= (taskDescName <$> existing)
                ]
            }
        , plainField "Description" (maybe "" (\(TaskDescription _ d _ _ _) -> d) existing)
        ]
    , formParse = \case
        [name, resource, duration, depends, description] -> do
          name' <- requireName "Name" name
          resource' <- requireName "Resource" resource
          duration' <- parseDurationField duration
          let dependencies =
                fromString . Text.unpack
                  <$> filter (not . Text.null) (cleanName <$> Text.splitOn "," depends)
          pure . ElemTask $
            TaskDescription
              (fromString (Text.unpack name'))
              (Text.strip description)
              (fromString (Text.unpack resource'))
              duration'
              dependencies
        _ -> Left "wrong number of fields"
    }

-- | Trim whitespace and surrounding quotes: names are entered bare in forms.
cleanName :: Text -> Text
cleanName raw =
  let t = Text.strip raw
   in case Text.stripPrefix "\"" t >>= Text.stripSuffix "\"" of
        Just inner -> inner
        Nothing -> t

requireName :: Text -> Text -> Either Text Text
requireName label raw =
  let t = cleanName raw
   in if Text.null t then Left (label <> " must not be empty") else Right t

parseWord :: Text -> Text -> Either Text Word
parseWord label raw = case Text.Read.decimal (Text.strip raw) of
  Right (n, rest) | Text.null rest -> Right n
  _ -> Left (label <> " must be a whole number")

-- | @uniform a b@, @normal avg dev@ or @logNormal median dev@ — no aliases.
parseDistribution :: Text -> Either Text DurationD
parseDistribution raw = case parseDurationDescription (Text.unpack (Text.strip raw)) of
  Right (Right duration) -> Right duration
  Right (Left _) -> Left distributionHint
  Left _ -> Left distributionHint
 where
  distributionHint = "Distribution must be: uniform A B | normal AVG DEV | logNormal MEDIAN DEV"

-- | Like 'parseDistribution' but a duration alias name is also accepted.
parseDurationField :: Text -> Either Text (Either DurationAlias DurationD)
parseDurationField raw = case parseDurationDescription (Text.unpack (cleanName raw)) of
  Right result -> Right result
  Left _ -> Left "Duration must be a distribution (uniform A B | normal AVG DEV | logNormal MEDIAN DEV) or the name of a duration alias"
