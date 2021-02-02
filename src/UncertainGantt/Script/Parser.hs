{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module UncertainGantt.Script.Parser (
  Statement (..),
  ResourceDescription (..),
  TaskDescription (..),
  Resource (..),
  DurationD (..),
  parseScript,
  MoreInputExpected (..),
) where

import Control.Monad (void)
import Data.Foldable qualified as F
import Data.Maybe qualified as Maybe
import Data.Monoid (First (First, getFirst))
import Data.Set qualified as Set
import Data.String (IsString)
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Lexer
import UncertainGantt.Task (TaskName (..))

newtype Resource = Resource String
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

data DurationD
  = UniformD Word Word
  | NormalD Double Double
  | LogNormalD Double Double
  deriving stock (Eq, Ord, Show)

data Statement
  = AddTask TaskDescription
  | AddResource ResourceDescription
  | PrintExample
  | PrintDescriptions
  | RunSimulations Word
  | PrintCompletionTimes
  | PrintCompletionTimeQuantile Word Word
  | PrintCompletionTimeMean
  deriving stock (Eq, Ord, Show)

data TaskDescription = TaskDescription TaskName String Resource DurationD [TaskName]
  deriving stock (Eq, Ord, Show)
data ResourceDescription = ResourceDescription Resource Word
  deriving stock (Eq, Ord, Show)

parseScript :: String -> Either (String, Maybe MoreInputExpected) [Statement]
parseScript s = case P.parse statements "" s of
  Left errors -> Left (P.errorBundlePretty errors, moreInputExpected errors)
  Right statements' -> Right statements'
 where
  statements = Maybe.catMaybes <$> P.many statement
  moreInputExpected :: P.ParseErrorBundle String MoreInputExpected -> Maybe MoreInputExpected
  moreInputExpected =
    getFirst
      . F.foldMap'
        ( \case
            P.ErrorCustom e -> First (Just e)
            _ -> mempty
        )
      . F.foldMap'
        ( \case
            P.FancyError _ fancyErrors -> fancyErrors
            _ -> mempty
        )
      . P.bundleErrors

data MoreInputExpected = ExpectedMultilineInput
  deriving stock (Eq, Ord, Show)

instance P.ShowErrorComponent MoreInputExpected where
  showErrorComponent _ = ""

type Parser a = P.Parsec MoreInputExpected String a

duration :: Parser DurationD
duration = F.asum [uniform, normal, logNormal]
 where
  uniform = do
    _ <- P.try $ P.Char.string "uniform"
    from <- P.Char.hspace1 *> P.Lexer.decimal
    to <- P.Char.hspace1 *> P.Lexer.decimal
    pure $ UniformD from to
  normal = do
    _ <- P.try $ P.Char.string "normal"
    average <- P.Char.hspace1 *> P.Lexer.float
    stddev <- P.Char.hspace1 *> P.Lexer.float
    pure $ NormalD average stddev
  logNormal = do
    _ <- P.try $ P.Char.string "logNormal"
    average <- P.Char.hspace1 *> P.Lexer.float
    stddev <- P.Char.hspace1 *> P.Lexer.float
    pure $ LogNormalD average stddev

newline :: Parser ()
newline = void $ P.Char.hspace *> P.Char.newline

statement :: Parser (Maybe Statement)
statement =
  F.asum
    [ Nothing <$ comment
    , Nothing <$ newline
    , Just . AddTask <$> taskDescription
    , Just . AddResource <$> resourceDescription
    , Just <$> printExample
    , Just <$> printDescriptions
    , Just <$> printCompletionTimes
    , Just <$> runSimulations
    , Just <$> printAverage
    , Just <$> printQuantile
    , Just <$> printPercentile
    ]
 where
  comment = P.try $ P.Lexer.skipLineComment "#"
  printExample =
    PrintExample <$ P.try (P.Char.string "print example")
  printDescriptions = do
    PrintDescriptions <$ P.try (P.Char.string "print descriptions")
  printCompletionTimes = do
    PrintCompletionTimes <$ P.try (P.Char.string "print times")
  runSimulations = do
    _ <- P.try $ P.Char.string "run simulations "
    RunSimulations <$> P.Lexer.decimal
  printAverage =
    PrintCompletionTimeMean <$ P.try (P.Char.string "print mean")
  printQuantile = do
    _ <- P.try $ P.Char.string "print quantile "
    PrintCompletionTimeQuantile
      <$> P.Lexer.decimal
      <*> do
        _ <- P.Char.hspace1 *> P.Char.string "of" <* P.Char.hspace1
        P.Lexer.decimal
  printPercentile = do
    _ <- P.try $ P.Char.string "print p"
    PrintCompletionTimeQuantile <$> P.Lexer.decimal <*> pure 100

resourceDescription :: Parser ResourceDescription
resourceDescription = do
  _ <- P.try $ P.Char.string "resource"
  P.Char.space1
  resName <- resource
  P.Char.space1
  resAmount <- P.Lexer.decimal
  newline
  pure $ ResourceDescription resName resAmount
 where
  resource = fmap Resource $ stringLiteral <|> name

taskDescription :: Parser TaskDescription
taskDescription = do
  _ <- P.try $ P.Char.string "task"
  taskName' <- P.Char.hspace1 *> taskName <* newline
  resource' <-
    P.label "resource name" $
      (tab `onEOFExpect` ExpectedMultilineInput)
        *> resource <* newline
  duration' <-
    P.label "duration distribution: uniform, normal or logNormal" $
      tab *> duration <* newline
  dependencies' <-
    P.try (P.label "dependencies list" $ tab *> dependencies <* newline)
      <|> pure []
  description <-
    P.try (P.label "task description" $ tab *> P.someTill P.Char.printChar P.Char.newline)
      <|> pure ""
  pure $ TaskDescription taskName' description resource' duration' dependencies'
 where
  tab = void $ P.Char.string "  "
  taskName = fmap TaskName $ stringLiteral <|> name
  resource = fmap Resource $ stringLiteral <|> name
  dependencies = do
    _ <- P.try $ P.Char.string "depends on"
    P.sepBy (P.Char.hspace1 *> taskName) (P.Char.hspace *> P.Char.char ',')

onEOFExpect :: Parser a -> MoreInputExpected -> Parser ()
onEOFExpect parser expectation =
  P.observing parser >>= \case
    Right _ -> pure ()
    Left (P.TrivialError _ (Just P.EndOfInput) _) ->
      P.fancyFailure . Set.singleton . P.ErrorCustom $ expectation
    Left otherError -> P.parseError otherError

name :: Parser String
name = P.some P.Char.alphaNumChar

stringLiteral :: Parser String
stringLiteral = P.Char.char '"' >> P.manyTill P.Lexer.charLiteral (P.Char.char '"')
