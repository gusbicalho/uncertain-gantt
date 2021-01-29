{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UncertainGantt.Script.Parser (
  Script (..),
  ProjectDefinition (..),
  ProjectItem (..),
  Query (..),
  Resource (..),
  DurationD (..),
  parseScript,
) where

import Control.Monad (void)
import Data.Foldable qualified as F
import Data.String (IsString)
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Lexer
import UncertainGantt.Task (TaskName (..))

data Script = Script
  { scriptProjectDefinition :: ProjectDefinition
  , scriptQueries :: [Query]
  }
  deriving stock (Eq, Ord, Show)

newtype ProjectDefinition = ProjectDefinition
  { projectItems :: [ProjectItem]
  }
  deriving stock (Eq, Ord, Show)

data ProjectItem
  = ResourceDecl Resource Word
  | TaskDecl TaskName String Resource DurationD [TaskName]
  deriving stock (Eq, Ord, Show)

newtype Resource = Resource String
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

data DurationD
  = UniformD Word Word
  deriving stock (Eq, Ord, Show)

data Query
  = PrintExample
  deriving stock (Eq, Ord, Show)

parseScript :: String -> Either String Script
parseScript s = case P.parse script "" s of
  Left errors -> Left $ P.errorBundlePretty errors
  Right script' -> Right script'

type Parser a = P.Parsec Void String a

script :: Parser Script
script = Script <$> projectDefinition <*> queriesSection
 where
  queriesSection = someQueries <|> ([] <$ P.eof)
  someQueries = P.try (P.some newline) *> queries <* P.eof

projectDefinition :: Parser ProjectDefinition
projectDefinition = ProjectDefinition <$> P.many projectItem

projectItem :: Parser ProjectItem
projectItem = resourceDecl <|> taskDecl
 where
  resourceDecl = do
    _ <- P.try $ P.Char.string "resource"
    P.Char.space1
    resName <- resource
    P.Char.space1
    resAmount <- P.Lexer.decimal
    newline
    pure $ ResourceDecl resName resAmount
  taskDecl = do
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
    pure $ TaskDecl taskName' description resource' duration' dependencies'
  name = P.some P.Char.alphaNumChar
  stringLiteral = P.Char.char '"' >> P.manyTill P.Lexer.charLiteral (P.Char.char '"')
  tab = void $ P.Char.string "  "
  taskName = fmap TaskName $ stringLiteral <|> name
  resource = fmap Resource $ stringLiteral <|> name
  dependencies = do
    _ <- P.try $ P.Char.string "depends on "
    P.Char.hspace
    P.sepBy taskName (P.Char.hspace *> P.Char.char ',' <* P.Char.hspace)

duration :: Parser DurationD
duration = F.asum [uniform]
 where
  uniform = do
    _ <- P.try $ P.Char.string "uniform"
    from <- P.Char.hspace1 *> P.Lexer.decimal
    to <- P.Char.hspace1 *> P.Lexer.decimal
    pure $ UniformD from to

newline :: Parser ()
newline = void $ P.Char.hspace *> P.Char.newline

queries :: Parser [Query]
queries = P.many query
 where
  query = F.asum . fmap (<* P.many newline) $ [printExample]
  printExample = do
    _ <- P.try $ P.Char.string "print example"
    pure PrintExample
