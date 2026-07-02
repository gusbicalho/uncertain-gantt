{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parser for the statement level of the @.ug@ script language. The
expression-level pieces (names, durations) live in
"UncertainGantt.Lang.Parser".
-}
module UncertainGantt.Script.Parser (parseScript) where

import Control.Monad (void)
import Data.Foldable qualified as F
import Data.Maybe qualified as Maybe
import Data.Monoid (First (First, getFirst))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Lexer
import UncertainGantt.Lang.Parser (duration, durationAlias, resource, taskName)
import UncertainGantt.Lang.Types (
  ResourceDescription (ResourceDescription),
  TaskDescription (TaskDescription),
 )
import UncertainGantt.Script.Types (
  MoreInputExpected (..),
  PrintGanttType (Average, Random),
  Statement (..),
 )

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
    , Just <$> durationAliasDecl
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
  durationAliasDecl = do
    _ <- P.try $ P.Char.string "duration"
    P.Char.hspace1
    alias <- durationAlias
    P.Char.hspace1
    DurationAliasDeclaration alias <$> duration
  printDuration = do
    _ <- P.try $ P.Char.string "print duration"
    P.Char.hspace1
    PrintDuration
      <$> F.asum
        [ Right <$> P.try duration
        , Left <$> durationAlias
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

taskDescription :: Parser TaskDescription
taskDescription = do
  _ <- P.try $ P.Char.string "task"
  taskName' <- P.Char.hspace1 *> taskName <* newline
  resource' <-
    P.label "resource name" $
      (tab `onEOFExpect` ExpectedMultilineInput)
        *> resource
        <* newline
  duration' <-
    P.label "duration distribution: uniform, normal or logNormal" $
      tab *> durationDescription <* newline
  dependencies' <-
    P.try (P.label "dependencies list" $ tab *> dependencies <* newline)
      <|> pure []
  description <-
    P.try (P.label "task description" $ fmap Text.pack $ tab *> P.someTill P.Char.printChar P.Char.newline)
      <|> pure ""
  pure $ TaskDescription taskName' description resource' duration' dependencies'
 where
  tab = void $ P.Char.string "  "
  durationDescription = (Right <$> duration) <|> (Left <$> durationAlias)
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
