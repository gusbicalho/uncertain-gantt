{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Browser-based project editor, mirroring uncertain-gantt-tui's feature
set (see ARCHITECTURE.md). Usage matches the TUI: @uncertain-gantt-web
FILE [PROJECT]@.
-}
module Main (main) where

import Data.Text qualified as Text
import Editor.Persistence (loadAppConfig)
import System.Environment (getArgs)
import System.Exit (die)
import Web.App (initGlobalState, page)
import Web.Hyperbole (liveApp, quickStartDocument, run, runPage)

webPort :: Int
webPort = 3000

main :: IO ()
main =
  getArgs >>= \case
    [] -> start "project.toml" Nothing
    [path] -> start path Nothing
    [path, project] -> start path (Just (Text.pack project))
    _ -> die "usage: uncertain-gantt-web [FILE [PROJECT]]"

start :: FilePath -> Maybe Text.Text -> IO ()
start path mbProject = do
  cfg <- loadAppConfig path mbProject
  initGlobalState cfg
  run webPort $ liveApp quickStartDocument (runPage page)
