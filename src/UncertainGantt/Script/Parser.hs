{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
  parseBlocks,
  ParseBlockFailure (..),
) where

import Control.Monad (void)
import Data.Foldable qualified as F
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Maybe qualified as Maybe
import Data.String (IsString)
import Data.Void (Void)
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

parseScript :: String -> Either String [Statement]
parseScript s = case runIdentity $ parseBlocks (Identity $ Just s) of
  Left (ParseLineError parseError) -> Left parseError
  Left NoInput -> Right []
  Right statements -> Right statements

data ParseBlockFailure = NoInput | ParseLineError String

parseBlocks :: forall m. Monad m => m (Maybe String) -> m (Either ParseBlockFailure [Statement])
parseBlocks nextBlock =
  nextBlock >>= \case
    Nothing -> pure $ Left NoInput
    Just s -> case P.parse statements "" s of
      Left errors -> pure . Left . ParseLineError $ P.errorBundlePretty errors
      Right statements' -> pure $ Right statements'
 where
  statements = Maybe.catMaybes <$> P.many statement

type Parser a = P.Parsec Void String a

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
  resource' <- tab *> resource <* newline
  duration' <- tab *> duration <* newline
  dependencies' <-
    P.try (tab *> dependencies <* newline)
      <|> pure []
  description <-
    P.try (tab *> P.someTill P.Char.printChar P.Char.newline)
      <|> pure ""
  pure $ TaskDescription taskName' description resource' duration' dependencies'
 where
  tab = void $ P.Char.string "  "
  taskName = fmap TaskName $ stringLiteral <|> name
  resource = fmap Resource $ stringLiteral <|> name
  dependencies = do
    _ <- P.try $ P.Char.string "depends on "
    P.Char.hspace
    P.sepBy taskName (P.Char.hspace *> P.Char.char ',' <* P.Char.hspace)

name :: Parser String
name = P.some P.Char.alphaNumChar

stringLiteral :: Parser String
stringLiteral = P.Char.char '"' >> P.manyTill P.Lexer.charLiteral (P.Char.char '"')
