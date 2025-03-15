{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module UncertainGantt.Script.Runner (
  runScript,
  runString,
  runFromFile,
  runFromHandle,
  runInteractive,
) where

import Control.Exception (Handler (Handler), catchJust, catches, throwIO)
import Control.Monad (join)
import Data.Bool (bool)
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.IORef qualified as IORef
import System.IO (Handle, IOMode (ReadMode), hFlush, hGetContents, hGetLine, hIsClosed, hPutStr, withFile)
import System.IO.Error (isEOFError, isUserError)
import UncertainGantt.Project (BuildProjectError)
import UncertainGantt.Script.Parser (parseScript)
import UncertainGantt.Script.StatementInterpreter (StatementInterpreter)
import UncertainGantt.Script.StatementInterpreter qualified as StatementInterpreter
import UncertainGantt.Script.Types (
  MoreInputExpected (..),
  Statement (..),
 )

runString ::
  (StatementInterpreter interpreter) =>
  String ->
  interpreter ->
  IO interpreter
runString scriptText interpreter =
  case parseScript scriptText of
    Left (parseError, _) -> throwIO . userError $ parseError
    Right script -> runScript script interpreter

runScript ::
  (StatementInterpreter interpreter) =>
  [Statement] ->
  interpreter ->
  IO interpreter
runScript statements interpreter = do
  interpreter_ <- IORef.newIORef interpreter
  F.traverse_ (execStatement interpreter_) statements
  IORef.readIORef interpreter_

runFromFile ::
  (StatementInterpreter interpreter) =>
  FilePath ->
  interpreter ->
  IO interpreter
runFromFile path interpreter = withFile path ReadMode $ \handle ->
  runFromHandle handle interpreter

runFromHandle ::
  (StatementInterpreter interpreter) =>
  Handle ->
  interpreter ->
  IO interpreter
runFromHandle handle interpreter = do
  safeGetContents <- once $ do
    hIsClosed handle >>= \case
      False -> Just <$> hGetContents handle
      True -> pure Nothing
  runBlocks (const (join <$> safeGetContents)) [] interpreter
 where
  once action = do
    done_ <- IORef.newIORef False
    pure $
      IORef.readIORef done_ >>= \case
        True -> pure Nothing
        False -> (Just <$> action) <* IORef.atomicWriteIORef done_ True

runInteractive ::
  (StatementInterpreter interpreter) =>
  Handle ->
  Handle ->
  interpreter ->
  IO interpreter
runInteractive handleIn handleOut = runBlocks getBlock errorHandlers
 where
  printError = putStrLn
  errorHandlers =
    [ Handler $ \(ex :: IOError) ->
        if isUserError ex
          then printError (show ex)
          else throwIO ex
    , Handler $ \(ex :: BuildProjectError) -> printError (show ex)
    ]
  getBlock expectation = getBlock' expectation <&> fmap (<> "\n")
  getBlock' Nothing = prompt "> "
  getBlock' (Just ExpectedMultilineInput) = Just . unlines <$> getMultilineBlock
  getMultilineBlock = do
    prompt "|   " >>= \case
      Nothing -> pure []
      Just "" -> pure []
      Just line -> ("  " <> line :) <$> getMultilineBlock
  prompt promptStr = do
    hPutStr handleOut promptStr
    hFlush handleOut
    (Just <$> hGetLine handleIn) `catchEOF` pure Nothing
  catchEOF action onEOF =
    catchJust (bool Nothing (Just ()) . isEOFError) action (const onEOF)

{-# INLINE runBlocks #-}
runBlocks ::
  (StatementInterpreter interpreter) =>
  (Maybe MoreInputExpected -> IO (Maybe String)) ->
  [Handler ()] ->
  interpreter ->
  IO interpreter
runBlocks getBlock errorHandlers interpreter = do
  interpreter_ <- IORef.newIORef interpreter
  go interpreter_ "" Nothing
  IORef.readIORef interpreter_
 where
  go interpreter_ prefix inputExpectation =
    getBlock inputExpectation >>= \case
      Nothing -> pure ()
      Just inputString -> case parseScript (prefix <> inputString) of
        Left (_, Just inputExpectation') ->
          go interpreter_ (prefix <> inputString) (Just inputExpectation')
        Left (parseError, _) -> do
          throwIO (userError parseError)
            `catches` errorHandlers
          go interpreter_ "" Nothing
        Right statements -> do
          F.traverse_ (execStatement interpreter_) statements
            `catches` errorHandlers
          go interpreter_ "" Nothing

{-# INLINE execStatement #-}
execStatement :: (StatementInterpreter interpreter) => IORef.IORef interpreter -> Statement -> IO ()
execStatement interpreter_ statement =
  IORef.readIORef interpreter_
    >>= StatementInterpreter.interpretStatement statement
    >>= IORef.atomicWriteIORef interpreter_
