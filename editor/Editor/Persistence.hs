{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Loading and saving a 'Doc' from a file path: dispatches between legacy
@.ug@ script persistence (one project per file) and TOML persistence (many
projects per file, selected by name). This is the UI-framework-agnostic
boundary shared by every editor frontend — it only decides where a 'Doc'
comes from and how it's saved back.

Two layers: 'loadDocument' / 'saveDocument' report failures as values, for
the web editor, which serves many documents and must not die because one
of them is malformed; 'loadAppConfig' wraps them for the TUI, which loads
a single document up front and can still exit on failure.

'saveDocument' re-reads the file rather than closing over the entries it
loaded, so two projects from the same TOML file can be open at once
without the second save clobbering the first.
-}
module Editor.Persistence (
  AppConfig (..),
  loadAppConfig,
  LoadedDoc (..),
  loadDocument,
  saveDocument,
  listProjectFiles,
) where

import Control.Monad (filterM)
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
import System.Directory (doesFileExist, listDirectory)
import System.Exit (die)
import System.FilePath (takeBaseName, (</>))
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

{- | A document read off disk, plus what the frontend needs to describe and
re-save it.
-}
data LoadedDoc = LoadedDoc
  { loadedDoc :: Doc
  , loadedProject :: Maybe Text
  {- ^ The project entry actually selected — always 'Nothing' for @.ug@
  scripts. Pass this back to 'saveDocument': it is resolved, where the
  name originally asked for may have been 'Nothing'.
  -}
  , loadedNote :: Maybe Text
  , loadedProjects :: [Text]
  -- ^ Every project in the file, for a frontend's project picker.
  }

-- | Is this the legacy script format? (Dispatch is by extension.)
isScript :: FilePath -> Bool
isScript = isSuffixOf ".ug"

-- | Project files we know how to open, sorted, directly in the directory.
listProjectFiles :: FilePath -> IO [FilePath]
listProjectFiles dir = do
  names <- listDirectory dir
  files <- filterM (\name -> doesFileExist (dir </> name)) names
  pure . List.sort $ filter isProjectFile files
 where
  isProjectFile name = ".toml" `isSuffixOf` name || isScript name

loadDocument :: FilePath -> Maybe Text -> IO (Either Text LoadedDoc)
loadDocument path mbProject
  | isScript path = loadScript path mbProject
  | otherwise = loadToml path mbProject

saveDocument :: FilePath -> Maybe Text -> Doc -> IO (Either Text Text)
saveDocument path mbProject doc
  | isScript path = saveScript path mbProject doc
  | otherwise = saveToml path mbProject doc

-- * Legacy @.ug@ script persistence: one project per file.

scriptHasNoProjects :: FilePath -> Text
scriptHasNoProjects path =
  Text.pack path <> " is a .ug script; it holds a single project, so a project name cannot be given"

loadScript :: FilePath -> Maybe Text -> IO (Either Text LoadedDoc)
loadScript path = \case
  Just _ -> pure . Left $ scriptHasNoProjects path
  Nothing -> do
    exists <- doesFileExist path
    if not exists
      then pure . Right $ loaded [] 0
      else do
        contents <- readFile path
        pure $ case parseScript contents of
          Left (err, _) -> Left ("Failed to parse " <> Text.pack path <> ":\n" <> Text.pack err)
          Right statements -> Right (uncurry loaded (fromStatements statements))
 where
  loaded doc dropped =
    LoadedDoc
      { loadedDoc = doc
      , loadedProject = Nothing
      , loadedNote =
          if dropped > (0 :: Int)
            then Just ("[" <> showText dropped <> " print/run statements ignored]")
            else Nothing
      , loadedProjects = []
      }

saveScript :: FilePath -> Maybe Text -> Doc -> IO (Either Text Text)
saveScript path mbProject doc = case mbProject of
  Just _ -> pure . Left $ scriptHasNoProjects path
  Nothing -> do
    Text.IO.writeFile path (renderDeclarations (toStatements doc))
    pure . Right $ "Saved " <> Text.pack path

-- * TOML persistence (see TOML-FORMAT.md): a file holds many projects;

-- we edit one and preserve the rest on save.

readEntries :: FilePath -> IO (Either Text [ProjectEntry])
readEntries path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      contents <- Text.IO.readFile path
      pure $ case decodeProjectsFile contents of
        Left err -> Left ("Failed to parse " <> Text.pack path <> ":\n" <> err)
        Right (ProjectsFile entries) -> Right entries

{- | The entry to edit: the one named, or the first if no name was given.
A file with no entries yet yields a fresh one, so a path that does not
exist opens as an empty project rather than an error.
-}
selectEntry :: FilePath -> Maybe Text -> [ProjectEntry] -> Either Text ProjectEntry
selectEntry path mbProject entries = case mbProject of
  Nothing -> case entries of
    [] -> Right (emptyProjectEntry (Text.pack (takeBaseName path)))
    (firstEntry : _) -> Right firstEntry
  Just projectName -> case List.find ((== projectName) . entryName) entries of
    Just found -> Right found
    Nothing
      | null entries -> Right (emptyProjectEntry projectName)
      | otherwise ->
          Left $
            "No project named \""
              <> projectName
              <> "\" in "
              <> Text.pack path
              <> ". Available: "
              <> Text.intercalate ", " (entryName <$> entries)

loadToml :: FilePath -> Maybe Text -> IO (Either Text LoadedDoc)
loadToml path mbProject =
  readEntries path >>= \case
    Left err -> pure (Left err)
    Right entries -> pure $ do
      entry <- selectEntry path mbProject entries
      Right
        LoadedDoc
          { loadedDoc = fromProjectEntry entry
          , loadedProject = Just (entryName entry)
          , loadedNote =
              if length entries > 1
                then Just ("[file has " <> showText (length entries) <> " projects — editing \"" <> entryName entry <> "\"]")
                else Nothing
          , loadedProjects = entryName <$> entries
          }

{- | Write the document back as one entry of the file, preserving the rest.
The file is re-read here so that concurrently open projects from the same
file each splice into the current contents instead of a stale snapshot;
the freshly read entry also carries metadata edited elsewhere since load.
-}
saveToml :: FilePath -> Maybe Text -> Doc -> IO (Either Text Text)
saveToml path mbProject doc =
  readEntries path >>= \case
    Left err -> pure (Left err)
    Right entries -> case selectEntry path mbProject entries of
      Left err -> pure (Left err)
      Right carrier -> do
        let entry = toProjectEntry carrier doc
        Text.IO.writeFile path (encodeProjectsFile (ProjectsFile (replaceOrAppend entry entries)))
        pure . Right $ "Saved " <> Text.pack path <> " · " <> entryName entry
 where
  replaceOrAppend entry entries
    | any ((== entryName entry) . entryName) entries =
        [if entryName old == entryName entry then entry else old | old <- entries]
    | otherwise = entries <> [entry]

-- * The single-document frontend (TUI)

{- | Load one document up front, exiting on failure. Save errors surface as
the status message the frontend already displays.
-}
loadAppConfig :: FilePath -> Maybe Text -> IO AppConfig
loadAppConfig path mbProject =
  loadDocument path mbProject >>= \case
    Left err -> die (Text.unpack err)
    Right loaded ->
      pure
        AppConfig
          { appTitle = maybe id (\name t -> t <> " · " <> name) (loadedProject loaded) (Text.pack path)
          , appInitialDoc = loadedDoc loaded
          , appInitialNote = loadedNote loaded
          , appSave = fmap (either id id) . saveDocument path (loadedProject loaded)
          }
