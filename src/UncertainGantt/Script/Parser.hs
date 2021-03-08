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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module UncertainGantt.Script.Parser (parseScript) where

import Control.Monad (void)
import Data.Foldable qualified as F
import Data.Maybe qualified as Maybe
import Data.Monoid (First (First, getFirst))
import Data.Set qualified as Set
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Lexer
import UncertainGantt.Script.Types (
  DurationAST (..),
  DurationD,
  MoreInputExpected (..),
  PrintGanttType (Average, Random),
  Resource (..),
  ResourceDescription (..),
  Statement (..),
  TaskDescription (..), DurationExpr
 )
import UncertainGantt.Task (TaskName (..))

parseScript :: String -> Either (String, Maybe MoreInputExpected) [Statement]
parseScript s = case P.parse statements "" s of
  Left errors -> Left (P.errorBundlePretty errors, moreInputExpected errors)
  Right statements' -> Right statements'
 where
  statements = Maybe.catMaybes <$> P.manyTill statement P.eof
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

type Parser a = P.Parsec MoreInputExpected String a

newline :: Parser ()
newline = void $ P.Char.hspace *> P.Char.newline

statement :: Parser (Maybe Statement)
statement =
  F.asum
    [ Nothing <$ comment
    , Nothing <$ newline
    , Just . AddTask <$> taskDescription
    , Just . AddResource <$> resourceDescription
    , Just <$> durationAlias
    , Just <$> printDuration
    , Just <$> printExample
    , Just <$> printRun
    , Just <$> printTasks
    , Just <$> printCompletionTimes
    , Just <$> runSimulations
    , Just <$> printMean
    , Just <$> printQuantile
    , Just <$> printPercentile
    , Just <$> printHistogram
    , fail "Unknown statement"
    ]
 where
  comment = P.try $ P.Lexer.skipLineComment "#"
  durationAlias = do
    _ <- P.try $ P.Char.string "duration"
    P.Char.hspace1
    alias <- stringLiteral <|> name
    P.Char.hspace1
    DurationAliasDeclaration alias <$> duration
  printDuration = do
    _ <- P.try $ P.Char.string "print duration"
    P.Char.hspace1
    PrintDuration
      <$> F.asum
        [ Right <$> P.try duration
        , Left <$> (stringLiteral <|> name)
        ]
  printExample =
    PrintGantt Random <$ P.try (P.Char.string "print example")
  printRun = do
    _ <- P.try (P.Char.string "print run")
    PrintGantt
      <$> F.asum
        [ Average <$ P.Char.string " average"
        , Random <$ P.Char.string " random"
        ]
  printTasks = do
    _ <- P.try (P.Char.string "print tasks")
    PrintTasks . Maybe.isJust <$> P.optional (P.Char.string " briefly")
  printCompletionTimes = do
    PrintCompletionTimes <$ P.try (P.Char.string "print times")
  runSimulations = do
    _ <- P.try $ P.Char.string "run simulations"
    P.Char.hspace1
    RunSimulations <$> P.Lexer.decimal
  printMean =
    PrintCompletionTimeMean <$ P.try (P.Char.string "print mean")
  printQuantile = do
    _ <- P.try $ P.Char.string "print quantile"
    P.Char.hspace1
    PrintCompletionTimeQuantile
      <$> P.Lexer.decimal
      <*> do
        _ <- P.Char.hspace1 *> P.Char.string "of" <* P.Char.hspace1
        P.Lexer.decimal
  printPercentile = do
    _ <- P.try $ P.Char.string "print p"
    PrintCompletionTimeQuantile <$> P.Lexer.decimal <*> pure 100
  printHistogram = do
    _ <- P.try $ P.Char.string "print histogram"
    P.Char.hspace1
    PrintHistogram <$> P.Lexer.decimal

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
    P.label "duration distribution" $
      tab *> durationDescription <* newline
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
  durationDescription = (Right <$> duration) <|> (Left <$> (stringLiteral <|> name))
  dependencies = do
    _ <- P.try $ P.Char.string "depends on"
    P.sepBy (P.Char.hspace1 *> taskName) (P.Char.hspace *> P.Char.char ',')

durationExpr :: Parser DurationExpr
-- TODO parse duration expressions
durationExpr = _

duration :: Parser DurationD
duration =
  F.asum
    [ uniform
    , normal
    , logNormal
    ]
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
