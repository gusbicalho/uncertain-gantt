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
import UncertainGantt.Script.Types (
  MoreInputExpected (..),
  Statement (..),
 )
import Utils.Agent qualified as Agent

runString ::
  Agent.AgentOn agent Statement IO =>
  String ->
  agent ->
  IO agent
runString scriptText agent =
  case parseScript scriptText of
    Left (parseError, _) -> throwIO . userError $ parseError
    Right script -> runScript script agent

runScript ::
  Agent.AgentOn agent Statement IO =>
  [Statement] ->
  agent ->
  IO agent
runScript statements agent = do
  agent_ <- IORef.newIORef agent
  F.traverse_ (execStatement agent_) statements
  IORef.readIORef agent_

runFromFile ::
  Agent.AgentOn agent Statement IO =>
  FilePath ->
  agent ->
  IO agent
runFromFile path agent = withFile path ReadMode $ \handle ->
  runFromHandle handle agent

runFromHandle ::
  Agent.AgentOn agent Statement IO =>
  Handle ->
  agent ->
  IO agent
runFromHandle handle agent = do
  safeGetContents <- once $ do
    hIsClosed handle >>= \case
      False -> Just <$> hGetContents handle
      True -> pure Nothing
  runBlocks (const (join <$> safeGetContents)) [] agent
 where
  once action = do
    done_ <- IORef.newIORef False
    pure $
      IORef.readIORef done_ >>= \case
        True -> pure Nothing
        False -> (Just <$> action) <* IORef.atomicWriteIORef done_ True

runInteractive ::
  Agent.AgentOn agent Statement IO =>
  Handle ->
  Handle ->
  agent ->
  IO agent
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
  Agent.AgentOn agent Statement IO =>
  (Maybe MoreInputExpected -> IO (Maybe String)) ->
  [Handler ()] ->
  agent ->
  IO agent
runBlocks getBlock errorHandlers agent = do
  agent_ <- IORef.newIORef agent
  go agent_ "" Nothing
  IORef.readIORef agent_
 where
  go agent_ prefix inputExpectation =
    getBlock inputExpectation >>= \case
      Nothing -> pure ()
      Just inputString -> case parseScript (prefix <> inputString) of
        Left (_, Just inputExpectation') ->
          go agent_ (prefix <> inputString) (Just inputExpectation')
        Left (parseError, _) -> do
          throwIO (userError parseError)
            `catches` errorHandlers
          go agent_ "" Nothing
        Right statements -> do
          F.traverse_ (execStatement agent_) statements
            `catches` errorHandlers
          go agent_ "" Nothing

{-# INLINE execStatement #-}
execStatement :: Agent.AgentOn agent stmt IO => IORef.IORef agent -> stmt -> IO ()
execStatement agent_ statement =
  IORef.readIORef agent_
    >>= Agent.run statement
    >>= IORef.atomicWriteIORef agent_
