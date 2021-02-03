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

import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (stdin, stdout)
import UncertainGantt qualified as UG

main :: IO ()
main = do
  args <- getArgs
  _ <- case args of
    [param]
      | isHelpOpt param -> help
      | isInteractiveOpt param ->
        UG.initialState >>= UG.runInteractive stdin stdout
      | param == "-" ->
        UG.initialState >>= UG.runFromHandle stdin
      | otherwise ->
        UG.initialState >>= UG.runFromFile param
    [path, option]
      | isInteractiveOpt option ->
        UG.initialState >>= UG.runFromFile path >>= UG.runInteractive stdin stdout
    _ -> badUsage
  pure ()
 where
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
