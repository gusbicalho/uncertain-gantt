{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Browser-based project editor (see ARCHITECTURE.md). Usage:
@uncertain-gantt-web [--port PORT] [PATH [PROJECT]]@, where @PATH@ is a
directory to browse or a single file to open; @PROJECT@ names an entry
within a multi-project TOML file.

Unlike the TUI, the server holds any number of documents open at once —
which one a page shows comes from its URL (see "Web.Route").
-}
module Main (main) where

import Data.Text (Text)
import Effectful.Reader.Dynamic (runReader)
import Options.Applicative (
  Parser,
  argument,
  auto,
  execParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  optional,
  progDesc,
  showDefault,
  str,
  value,
  (<**>),
 )
import System.Directory (doesDirectoryExist)
import System.Exit (die)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Web.App (app)
import Web.Hyperbole (liveApp, quickStartDocument, run)
import Web.Route (DocKey (DocKey))
import Web.State (newAdapters)

data Options = Options
  { optPort :: Int
  , optPath :: FilePath
  , optProject :: Maybe Text
  }

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "port"
          <> metavar "PORT"
          <> value 3000
          <> showDefault
          <> help "Port to serve the editor on"
      )
    <*> argument
      str
      ( metavar "PATH"
          <> value "."
          <> showDefault
          <> help "Directory of project files to browse, or a single file to open"
      )
    <*> optional
      ( argument
          str
          ( metavar "PROJECT"
              <> help "Name of the project entry to edit (TOML files hold many; defaults to the first)"
          )
      )

main :: IO ()
main = do
  -- So the startup line shows up promptly even when stdout is a pipe.
  hSetBuffering stdout LineBuffering
  opts <-
    execParser $
      info
        (options <**> helper)
        (fullDesc <> progDesc "Browser-based editor for uncertain-gantt project files")
  (root, startup) <- resolveTarget (optPath opts) (optProject opts)
  adapters <- newAdapters root startup
  putStrLn $
    "uncertain-gantt-web: serving "
      <> root
      <> " on http://localhost:"
      <> show (optPort opts)
      <> "/"
  run (optPort opts) $ liveApp quickStartDocument (runReader adapters app)

{- | The directory to serve, and the document to land on. A file argument
serves its directory so the browser can still reach its neighbours.
-}
resolveTarget :: FilePath -> Maybe Text -> IO (FilePath, Maybe DocKey)
resolveTarget path mbProject = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then case mbProject of
      Just _ -> die (path <> " is a directory; a project name only makes sense with a file")
      Nothing -> pure (path, Nothing)
    else pure (takeDirectory path, Just (DocKey (takeFileName path) mbProject))
