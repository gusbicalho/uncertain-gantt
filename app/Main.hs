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
import System.Exit (ExitCode (ExitFailure), exitWith)
import UncertainGantt qualified as UG

main :: IO ()
main =
  getArgs >>= \case
    [] -> badUsage
    (path : _) -> do
      UG.runFromFile path

badUsage :: IO ()
badUsage = do
  putStrLn "Usage: uncertain-gantt <script-file>"
  exitWith (ExitFailure (-1))
