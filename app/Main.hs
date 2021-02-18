{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Monad ((>=>))
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (stdin, stdout)
import UncertainGantt qualified as UG

main :: IO ()
main = do
  args <- getArgs
  state <- initialState
  _ <- dispatch args state
  pure ()
 where
  dispatch [param]
    | isHelpOpt param = const help
    | isInteractiveOpt param = runInteractive
    | param == "-" = runFromStdin
    | otherwise = runFromFile param
  dispatch [path, option]
    | isInteractiveOpt option = runFromFile path >=> runInteractive
  dispatch _ = const badUsage
  runner = UG.DefaultRunnerIO
  initialState = UG.initState runner
  runInteractive = UG.runInteractive stdin stdout runner
  runFromStdin = UG.runFromHandle stdin runner
  runFromFile path = UG.runFromFile path runner
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
  putStrLn "  uncertain-gantt <script-file> -i  # reads from file and enters interactive mode"
