{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Browser-based project editor, mirroring uncertain-gantt-tui's feature
set (see ARCHITECTURE.md). Usage matches the TUI, plus a port option:
@uncertain-gantt-web [--port PORT] [FILE [PROJECT]]@.
-}
module Main (main) where

import Data.Text (Text)
import Editor.Persistence (loadAppConfig)
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
import Web.App (initGlobalState, page)
import Web.Hyperbole (liveApp, quickStartDocument, run, runPage)

data Options = Options
  { optPort :: Int
  , optFile :: FilePath
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
      ( metavar "FILE"
          <> value "project.toml"
          <> showDefault
          <> help "Project file: .toml (primary) or legacy .ug"
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
  opts <-
    execParser $
      info
        (options <**> helper)
        (fullDesc <> progDesc "Browser-based editor for uncertain-gantt project files")
  cfg <- loadAppConfig (optFile opts) (optProject opts)
  initGlobalState cfg
  run (optPort opts) $ liveApp quickStartDocument (runPage page)
