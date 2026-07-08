{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Loading an 'AppConfig' from a file path: dispatches between legacy
@.ug@ script persistence (one project per file) and TOML persistence (many
projects per file, selected by name). This is the UI-framework-agnostic
boundary shared by every editor frontend — it only decides where a 'Doc'
comes from and how it's saved back.
-}
module Editor.Persistence (
  AppConfig (..),
  loadAppConfig,
) where

import Data.List (isSuffixOf)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Editor.Doc (
  Doc,
  fromProjectEntry,
  fromStatements,
  toProjectEntry,
  toStatements,
 )
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeBaseName)
import UncertainGantt.Script.Parser (parseScript)
import UncertainGantt.Script.Render (renderDeclarations)
import UncertainGantt.ToText (showText)
import UncertainGantt.Toml (
  ProjectEntry (entryName),
  ProjectsFile (ProjectsFile),
  decodeProjectsFile,
  emptyProjectEntry,
  encodeProjectsFile,
 )

-- | Everything an editor frontend needs to know about where the project came from.
data AppConfig = AppConfig
  { appTitle :: Text
  , appInitialDoc :: Doc
  , appInitialNote :: Maybe Text
  , appSave :: Doc -> IO Text
  }

loadAppConfig :: FilePath -> Maybe Text -> IO AppConfig
loadAppConfig path mbProject
  | ".ug" `isSuffixOf` path = loadScript path mbProject
  | otherwise = loadToml path mbProject

-- | Legacy @.ug@ script persistence: one project per file.
loadScript :: FilePath -> Maybe Text -> IO AppConfig
loadScript path mbProject = do
  case mbProject of
    Just _ -> die (path <> " is a .ug script; it holds a single project, so a project name cannot be given")
    Nothing -> pure ()
  exists <- doesFileExist path
  (doc0, dropped) <-
    if not exists
      then pure ([], 0)
      else do
        contents <- readFile path
        case parseScript contents of
          Left (err, _) -> die ("Failed to parse " <> path <> ":\n" <> err)
          Right statements -> pure (fromStatements statements)
  pure
    AppConfig
      { appTitle = Text.pack path
      , appInitialDoc = doc0
      , appInitialNote =
          if dropped > 0
            then Just ("[" <> showText dropped <> " print/run statements ignored]")
            else Nothing
      , appSave = \doc -> do
          Text.IO.writeFile path (renderDeclarations (toStatements doc))
          pure ("Saved " <> Text.pack path)
      }

{- | TOML persistence (see TOML-FORMAT.md): a file holds many projects;
we edit one and preserve the rest on save.
-}
loadToml :: FilePath -> Maybe Text -> IO AppConfig
loadToml path mbProject = do
  exists <- doesFileExist path
  entries <-
    if not exists
      then pure []
      else do
        contents <- Text.IO.readFile path
        case decodeProjectsFile contents of
          Left err -> die ("Failed to parse " <> path <> ":\n" <> Text.unpack err)
          Right (ProjectsFile entries) -> pure entries
  (index, entry) <- case mbProject of
    Nothing -> pure $ case entries of
      [] -> (0, emptyProjectEntry (Text.pack (takeBaseName path)))
      (first : _) -> (0, first)
    Just projectName ->
      case List.find ((== projectName) . entryName . snd) (zip [0 ..] entries) of
        Just found -> pure found
        Nothing
          | null entries -> pure (0, emptyProjectEntry projectName)
          | otherwise ->
              die . Text.unpack $
                "No project named \""
                  <> projectName
                  <> "\" in "
                  <> Text.pack path
                  <> ". Available: "
                  <> Text.intercalate ", " (entryName <$> entries)
  pure
    AppConfig
      { appTitle = Text.pack path <> " · " <> entryName entry
      , appInitialDoc = fromProjectEntry entry
      , appInitialNote =
          if length entries > 1
            then Just ("[file has " <> showText (length entries) <> " projects — editing \"" <> entryName entry <> "\"]")
            else Nothing
      , appSave = \doc -> do
          let entries' = setOrAppend index (toProjectEntry entry doc) entries
          Text.IO.writeFile path (encodeProjectsFile (ProjectsFile entries'))
          pure ("Saved " <> Text.pack path <> " · " <> entryName entry)
      }
 where
  setOrAppend i x xs
    | i < length xs = take i xs <> [x] <> drop (i + 1) xs
    | otherwise = xs <> [x]
