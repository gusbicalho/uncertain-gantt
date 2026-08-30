{-# LANGUAGE ImportQualifiedPost #-}

{- | Parsers for the expression-level pieces of the definition language:
names and duration distributions. Polymorphic in the megaparsec error
component so the script parser (with its custom continuation errors) and
standalone uses (e.g. TUI form fields) share one grammar.
-}
module UncertainGantt.Lang.Parser (
  parseDurationDescription,
  duration,
  durationAlias,
  taskName,
  resource,
  name,
  stringLiteral,
) where

import Data.Foldable qualified as F
import Data.Void (Void)
import Symbolize qualified
import Text.Megaparsec ((<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Lexer
import UncertainGantt.Lang.Types (
  DurationAlias (DurationAlias),
  DurationD (LogNormalD, NormalD, UniformD),
  Resource (Resource),
 )
import UncertainGantt.Task (TaskName (TaskName))

{- | Parse a standalone duration description: either a distribution like
@uniform 1 5@ or the name of a duration alias.
-}
parseDurationDescription :: String -> Either String (Either DurationAlias DurationD)
parseDurationDescription s =
  case P.parse (P.Char.hspace *> durationOrAlias <* P.Char.hspace <* P.eof) "" s of
    Left errors -> Left (P.errorBundlePretty errors)
    Right result -> Right result
 where
  durationOrAlias :: P.Parsec Void String (Either DurationAlias DurationD)
  durationOrAlias = (Right <$> duration) <|> (Left <$> durationAlias)

duration :: (Ord e) => P.Parsec e String DurationD
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
    average <- P.Char.hspace1 *> number
    stddev <- P.Char.hspace1 *> number
    pure $ NormalD average stddev
  logNormal = do
    _ <- P.try $ P.Char.string "logNormal"
    average <- P.Char.hspace1 *> number
    stddev <- P.Char.hspace1 *> number
    pure $ LogNormalD average stddev

-- | A decimal point is optional: both @10@ and @10.5@ are accepted.
number :: forall e. (Ord e) => P.Parsec e String Double
number = P.try P.Lexer.float <|> (fromIntegral <$> (P.Lexer.decimal :: P.Parsec e String Integer))

name :: (Ord e) => P.Parsec e String String
name = P.some P.Char.alphaNumChar

stringLiteral :: (Ord e) => P.Parsec e String String
stringLiteral = P.Char.char '"' >> P.manyTill P.Lexer.charLiteral (P.Char.char '"')

durationAlias :: (Ord e) => P.Parsec e String DurationAlias
durationAlias = DurationAlias . Symbolize.intern <$> (stringLiteral <|> name)

taskName :: (Ord e) => P.Parsec e String TaskName
taskName = TaskName . Symbolize.intern <$> (stringLiteral <|> name)

resource :: (Ord e) => P.Parsec e String Resource
resource = Resource . Symbolize.intern <$> (stringLiteral <|> name)
