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
  initState,
) where

import Control.Exception (Handler (Handler), catchJust, catches, throwIO)
import Control.Monad (join)
import Data.Bool (bool)
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.IORef qualified as IORef
import Data.Row.Variants qualified as Variants
import System.IO (Handle, IOMode (ReadMode), hFlush, hGetContents, hGetLine, hIsClosed, hPutStr, withFile)
import System.IO.Error (isEOFError, isUserError)
import UncertainGantt.Project (BuildProjectError)
import UncertainGantt.Script.Parser (parseScript)
import UncertainGantt.Script.Types (
  MoreInputExpected (..),
  RunStmt,
  Statement (..),
  initState
 )
import Utils.Runner qualified as Runner

runString :: RunStmt Statement runner s IO => String -> runner -> s -> IO s
runString scriptText runner state =
  case parseScript scriptText of
    Left (parseError, _) -> throwIO . userError $ parseError
    Right script -> runScript script runner state

runScript :: RunStmt Statement runner s IO => [Statement] -> runner -> s -> IO s
runScript statements runner state = do
  state_ <- IORef.newIORef state
  F.traverse_ (execStatement runner state_) statements
  IORef.readIORef state_

runFromFile :: RunStmt Statement runner s IO => FilePath -> runner -> s -> IO s
runFromFile path runner state = withFile path ReadMode $ \handle ->
  runFromHandle handle runner state

runFromHandle :: RunStmt Statement runner s IO => Handle -> runner -> s -> IO s
runFromHandle handle runner state = do
  safeGetContents <- once $ do
    hIsClosed handle >>= \case
      False -> Just <$> hGetContents handle
      True -> pure Nothing
  runBlocks (const (join <$> safeGetContents)) [] runner state
 where
  once action = do
    done_ <- IORef.newIORef False
    pure $
      IORef.readIORef done_ >>= \case
        True -> pure Nothing
        False -> (Just <$> action) <* IORef.atomicWriteIORef done_ True

runInteractive :: RunStmt Statement runner s IO => Handle -> Handle -> runner -> s -> IO s
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
  RunStmt Statement runner s IO =>
  (Maybe MoreInputExpected -> IO (Maybe String)) ->
  [Handler ()] ->
  runner ->
  s ->
  IO s
runBlocks getBlock errorHandlers runner state = do
  state_ <- IORef.newIORef state
  go state_ "" Nothing
  IORef.readIORef state_
 where
  go state_ prefix inputExpectation =
    getBlock inputExpectation >>= \case
      Nothing -> pure ()
      Just inputString -> case parseScript (prefix <> inputString) of
        Left (_, Just inputExpectation') ->
          go state_ (prefix <> inputString) (Just inputExpectation')
        Left (parseError, _) -> do
          throwIO (userError parseError)
            `catches` errorHandlers
          go state_ "" Nothing
        Right statements -> do
          F.traverse_ (execStatement runner state_) statements
            `catches` errorHandlers
          go state_ "" Nothing

{-# INLINE execStatement #-}
execStatement :: RunStmt stmt runner s IO => runner -> IORef.IORef s -> stmt -> IO ()
execStatement runner state_ statement =
  IORef.readIORef state_
    >>= runStatement statement runner
    >>= IORef.atomicWriteIORef state_

{-# INLINE runStatement #-}
runStatement ::
  RunStmt stmt runner state m =>
  stmt ->
  runner ->
  state ->
  m state
runStatement stmt runner (state :: state) =
  Variants.erase @((~) state) id
    <$> Runner.runVariant runner message
 where
  message = Variants.map' (state,) $ Variants.fromNative stmt
