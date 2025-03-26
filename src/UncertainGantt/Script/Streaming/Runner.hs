{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UncertainGantt.Script.Streaming.Runner (
  runScript,
  runScriptWith,
  promptAndParse,
  runInteractive,
  runInteractiveWith,
) where

import Control.Exception (catchJust)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Streaming (Of, Stream, lift)
import Streaming.Prelude qualified as S
import System.IO (Handle, hFlush, hGetLine, hPutStr)
import System.IO.Error (isEOFError)
import UncertainGantt.Script.Parser (parseScript)
import UncertainGantt.Script.Streaming.ConsoleInterpreter qualified as ConsoleInterpreter
import UncertainGantt.Script.Streaming.StatementInterpreter (interpretStream)
import UncertainGantt.Script.Types (MoreInputExpected (..), Statement (..))

-- | Run a script using the streaming interpreter
runScript :: Text.Text -> IO ConsoleInterpreter.StreamingConsoleInterpreter
runScript script = ConsoleInterpreter.new >>= runScriptWith script

-- | Run a script with a specific initial interpreter state
runScriptWith ::
  Text.Text ->
  ConsoleInterpreter.StreamingConsoleInterpreter ->
  IO ConsoleInterpreter.StreamingConsoleInterpreter
runScriptWith script interpreter = do
  case parseScript (Text.unpack script) of
    Left (parseError, _) -> do
      Text.IO.putStrLn $ "Error parsing script: " <> Text.pack parseError
      pure interpreter
    Right statements -> do
      -- Interpret the statements and return the final state
      result <-
        S.effects $
          S.mapM Text.IO.putStrLn $
            interpretStream interpreter (traverse_ S.yield statements)
      pure result

-- | Run an interactive session using the streaming interpreter
runInteractive ::
  Handle ->
  Handle ->
  IO ConsoleInterpreter.StreamingConsoleInterpreter
runInteractive inHandle outHandle =
  ConsoleInterpreter.new >>= runInteractiveWith inHandle outHandle

-- | Run an interactive session with a specific initial interpreter state
runInteractiveWith ::
  Handle ->
  Handle ->
  ConsoleInterpreter.StreamingConsoleInterpreter ->
  IO ConsoleInterpreter.StreamingConsoleInterpreter
runInteractiveWith handleIn handleOut interpreter = do
  S.effects $
    S.mapM Text.IO.putStrLn $
      interpretStream interpreter (promptAndParse handleIn handleOut)

-- | Prompt for input, read from terminal, and parse into a stream of statements
promptAndParse :: Handle -> Handle -> Stream (Of Statement) IO ()
promptAndParse handleIn handleOut = go "" Nothing
 where
  go prefix inputExpectation = do
    input <- lift $ getBlock inputExpectation
    case input of
      Nothing -> pure ()
      Just inputString ->
        case parseScript (prefix <> inputString) of
          Left (_, Just inputExpectation') ->
            go (prefix <> inputString) (Just inputExpectation')
          Left (parseError, _) -> do
            lift $ Text.IO.hPutStrLn handleOut (Text.pack $ "Error: " <> parseError)
            go "" Nothing
          Right statements -> do
            traverse_ S.yield statements
            go "" Nothing

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
