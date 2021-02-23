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

runString :: String -> Agent.AgentOn Statement IO -> IO (Agent.AgentOn Statement IO)
runString scriptText runner =
  case parseScript scriptText of
    Left (parseError, _) -> throwIO . userError $ parseError
    Right script -> runScript script runner

runScript :: [Statement] -> Agent.AgentOn Statement IO -> IO (Agent.AgentOn Statement IO)
runScript statements runner = do
  runner_ <- IORef.newIORef runner
  F.traverse_ (execStatement runner_) statements
  IORef.readIORef runner_

runFromFile :: FilePath -> Agent.AgentOn Statement IO -> IO (Agent.AgentOn Statement IO)
runFromFile path runner = withFile path ReadMode $ \handle ->
  runFromHandle handle runner

runFromHandle :: Handle -> Agent.AgentOn Statement IO -> IO (Agent.AgentOn Statement IO)
runFromHandle handle runner = do
  safeGetContents <- once $ do
    hIsClosed handle >>= \case
      False -> Just <$> hGetContents handle
      True -> pure Nothing
  runBlocks (const (join <$> safeGetContents)) [] runner
 where
  once action = do
    done_ <- IORef.newIORef False
    pure $
      IORef.readIORef done_ >>= \case
        True -> pure Nothing
        False -> (Just <$> action) <* IORef.atomicWriteIORef done_ True

runInteractive :: Handle -> Handle -> Agent.AgentOn Statement IO -> IO (Agent.AgentOn Statement IO)
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
  (Maybe MoreInputExpected -> IO (Maybe String)) ->
  [Handler ()] ->
  Agent.AgentOn Statement IO ->
  IO (Agent.AgentOn Statement IO)
runBlocks getBlock errorHandlers runner = do
  runner_ <- IORef.newIORef runner
  go runner_ "" Nothing
  IORef.readIORef runner_
 where
  go runner_ prefix inputExpectation =
    getBlock inputExpectation >>= \case
      Nothing -> pure ()
      Just inputString -> case parseScript (prefix <> inputString) of
        Left (_, Just inputExpectation') ->
          go runner_ (prefix <> inputString) (Just inputExpectation')
        Left (parseError, _) -> do
          throwIO (userError parseError)
            `catches` errorHandlers
          go runner_ "" Nothing
        Right statements -> do
          F.traverse_ (execStatement runner_) statements
            `catches` errorHandlers
          go runner_ "" Nothing

{-# INLINE execStatement #-}
execStatement :: IORef.IORef (Agent.AgentOn stmt IO) -> stmt -> IO ()
execStatement runner_ statement =
  IORef.readIORef runner_
    >>= flip Agent.run statement
    >>= IORef.atomicWriteIORef runner_
