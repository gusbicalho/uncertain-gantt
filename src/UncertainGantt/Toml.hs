{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The TOML project file format: a file holds any number of projects,
each with free-form string metadata that tools preserve without
interpreting. See TOML-FORMAT.md for the format specification and
evolution rules.
-}
module UncertainGantt.Toml (
  ProjectsFile (..),
  ProjectEntry (..),
  emptyProjectEntry,
  projectsFileCodec,
  decodeProjectsFile,
  encodeProjectsFile,
) where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Symbolize (Symbol)
import Toml (TomlCodec, (.=))
import Toml qualified
import UncertainGantt.Lang.Parser (parseDurationDescription)
import UncertainGantt.Lang.Render (renderDuration, renderName)
import UncertainGantt.Lang.Types (
  DurationAlias,
  DurationD,
  Resource,
  ResourceDescription (ResourceDescription),
  TaskDescription (TaskDescription),
  unDurationAlias,
  unResource,
 )
import UncertainGantt.Task (TaskName, unTaskName)
import UncertainGantt.ToText (ToText (toText))

-- | One project in a file.
data ProjectEntry = ProjectEntry
  { entryName :: Text
  , entryMeta :: Map Text Text
  -- ^ Free-form metadata; tools preserve it verbatim.
  , entryResources :: [ResourceDescription]
  , entryDurations :: [(DurationAlias, DurationD)]
  , entryTasks :: [TaskDescription]
  }
  deriving stock (Eq, Show)

emptyProjectEntry :: Text -> ProjectEntry
emptyProjectEntry name =
  ProjectEntry
    { entryName = name
    , entryMeta = Map.empty
    , entryResources = []
    , entryDurations = []
    , entryTasks = []
    }

newtype ProjectsFile = ProjectsFile {projectsFileEntries :: [ProjectEntry]}
  deriving stock (Eq, Show)

projectsFileCodec :: TomlCodec ProjectsFile
projectsFileCodec = Toml.diwrap (Toml.list projectEntryCodec "project")

decodeProjectsFile :: Text -> Either Text ProjectsFile
decodeProjectsFile = first Toml.prettyTomlDecodeErrors . Toml.decode projectsFileCodec

encodeProjectsFile :: ProjectsFile -> Text
encodeProjectsFile = Toml.encode projectsFileCodec

projectEntryCodec :: TomlCodec ProjectEntry
projectEntryCodec =
  ProjectEntry
    <$> Toml.text "name" .= entryName
    <*> withDefault Map.empty (Toml.tableMap Toml._KeyText Toml.text "meta") .= entryMeta
    <*> Toml.list resourceCodec "resource" .= entryResources
    <*> Toml.list durationAliasCodec "duration" .= entryDurations
    <*> Toml.list taskCodec "task" .= entryTasks

resourceCodec :: TomlCodec ResourceDescription
resourceCodec =
  ResourceDescription
    <$> nameCodec unResource "name" .= (\(ResourceDescription r _) -> r)
    <*> Toml.word "capacity" .= (\(ResourceDescription _ c) -> c)

durationAliasCodec :: TomlCodec (DurationAlias, DurationD)
durationAliasCodec =
  (,)
    <$> nameCodec unDurationAlias "name" .= fst
    <*> Toml.textBy renderDuration parseDistribution "distribution" .= snd
 where
  parseDistribution raw = case parseDurationDescription (Text.unpack raw) of
    Right (Right distribution) -> Right distribution
    Right (Left _) ->
      Left "expected a distribution (uniform A B | normal AVG DEV | logNormal MEDIAN DEV), not an alias name"
    Left err -> Left (Text.pack err)

taskCodec :: TomlCodec TaskDescription
taskCodec =
  mkTask
    <$> nameCodec unTaskName "name" .= (\(TaskDescription n _ _ _ _) -> n)
    <*> nameCodec unResource "resource" .= (\(TaskDescription _ _ r _ _) -> r)
    <*> Toml.textBy renderDurationRef parseDurationRef "duration" .= (\(TaskDescription _ _ _ d _) -> d)
    <*> withDefault [] (taskNameList "after") .= (\(TaskDescription _ _ _ _ deps) -> deps)
    <*> withDefault "" (Toml.text "description") .= (\(TaskDescription _ d _ _ _) -> d)
 where
  mkTask name resource duration after description =
    TaskDescription name description resource duration after
  renderDurationRef = either (renderName . unDurationAlias) renderDuration
  parseDurationRef raw =
    first Text.pack (parseDurationDescription (Text.unpack raw))
  taskNameList =
    Toml.dimap (fmap (toText . unTaskName)) (fmap (fromString . Text.unpack))
      . Toml.arrayOf Toml._Text

{- | Names are plain TOML strings, interned on read. The type covers
'Resource', 'DurationAlias' and 'TaskName' via their 'IsString'
instances and their unwrap-to-'Symbol' accessors.
-}
nameCodec :: (IsString name) => (name -> Symbol) -> Toml.Key -> TomlCodec name
nameCodec unwrap =
  Toml.textBy
    (toText . unwrap)
    ( \raw ->
        if Text.null (Text.strip raw)
          then Left "name must not be empty"
          else Right (fromString (Text.unpack raw))
    )

-- | The key is absent when the value equals the default.
withDefault :: (Eq a) => a -> TomlCodec a -> TomlCodec a
withDefault def codec =
  Toml.dimap
    (\a -> if a == def then Nothing else Just a)
    (fromMaybe def)
    (Toml.dioptional codec)
