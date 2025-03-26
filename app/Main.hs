{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Data.Functor (($>))
import Data.Text.IO qualified as Text.IO
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (stdin, stdout)
import UncertainGanttStreaming qualified as UGS

main :: IO ()
main = do
  args <- getArgs
  dispatch args
 where
  dispatch [param]
    | isHelpOpt param = help
    | isInteractiveOpt param = runInteractive
    | param == "-" = runFromStdin
    | otherwise = runFromFile param $> ()
  dispatch [path, option]
    | isInteractiveOpt option = do
        -- Run file first and get the final interpreter state
        interpreter <- runFromFile path
        -- Then start interactive mode with that interpreter state
        _ <- UGS.runInteractiveWith stdin stdout interpreter
        pure ()
  dispatch _ = badUsage

  runInteractive = do
    -- Use the streaming implementation for interactive mode
    _ <- UGS.runInteractive stdin stdout
    pure ()

  runFromStdin = do
    content <- Text.IO.getContents
    _ <- UGS.runScript content
    pure ()

  runFromFile path = do
    putStrLn $ "Processing file: " ++ path
    content <- Text.IO.readFile path
    UGS.runScript content

  isHelpOpt s = s == "--help"
  isInteractiveOpt s = s `elem` ["-i", "--interactive"]

help :: IO a
help = printUsage *> exitSuccess

badUsage :: IO a
badUsage = printUsage *> exitWith (ExitFailure (-1))

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "  uncertain-gantt -                 # reads from stdin in file mode"
  putStrLn "  uncertain-gantt --interactive     # (-i) interactive mode"
  putStrLn "  uncertain-gantt <script-file>     # reads from file"
  putStrLn "  uncertain-gantt <script-file> -i  # reads from file and enters interactive mode (not yet implemented in streaming mode)"
