{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lispy.Parser (
  Form (..),
  Bracket (..),
  Error (..),
  parse,
) where

import Data.Foldable qualified as F
import Data.Monoid (First (First, getFirst))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Lexer
import UncertainGantt.Script.Types (MoreInputExpected (ExpectedMultilineInput))

data Form
  = LitString P.SourcePos Text
  | LitInteger P.SourcePos Integer
  | LitDouble P.SourcePos Double
  | Symbol P.SourcePos Text
  | Comment P.SourcePos Text
  | Bracketed P.SourcePos Bracket [Form]
  deriving stock (Show)

data Bracket
  = Parens
  | SquareBracket
  | CurlyBracket
  deriving stock (Show)

newtype Error = MkError Text
  deriving stock (Show)

parse :: FilePath -> Text -> Either (Error, Maybe MoreInputExpected) [Form]
parse sourceName content = case P.parse program sourceName content of
  Left errors -> Left (MkError . Text.pack $ P.errorBundlePretty errors, moreInputExpected errors)
  Right statements' -> Right statements'
 where
  moreInputExpected :: P.ParseErrorBundle Text MoreInputExpected -> Maybe MoreInputExpected
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

type Parser a = P.Parsec MoreInputExpected Text a

program :: Parser [Form]
program = forms <* P.eof

forms :: Parser [Form]
forms = P.many do
  f <- form
  P.Char.space
  pure f

form :: Parser Form
form = do
  sourcePos <- P.getSourcePos
  P.choice
    [ litString sourcePos
    , litInteger sourcePos
    , litDouble sourcePos
    , symbol sourcePos
    , bracketed sourcePos
    , comment sourcePos
    ]

litString :: P.SourcePos -> Parser Form
litString sourcePos = do
  chars <- P.between openQuote closeQuote $ P.many nonQuote
  let text = Text.pack chars
  pure $ LitString sourcePos text
 where
  openQuote = P.Char.char '"'
  closeQuote = P.Char.char '"' P.<|> onEOFFailExpectingMore
  nonQuote =
    P.choice
      [ P.anySingleBut '"'
      , P.Char.string "\"" *> pure '"'
      ]

litInteger :: P.SourcePos -> Parser Form
litInteger sourcePos = do
  num <- signed P.Lexer.decimal
  P.notFollowedBy P.Char.letterChar
  pure $ LitDouble sourcePos num

litDouble :: P.SourcePos -> Parser Form
litDouble sourcePos = do
  num <- signed P.Lexer.float
  P.notFollowedBy P.Char.letterChar
  pure $ LitDouble sourcePos num

symbol :: P.SourcePos -> Parser Form
symbol sourcePos = do
  chars <-
    P.some
      ( P.choice
          [ P.Char.alphaNumChar
          , P.oneOf
              [ '+'
              , '-'
              , '_'
              , '&'
              , '%'
              , '$'
              , '>'
              , '<'
              ]
          ]
      )
  let text = Text.pack chars
  pure $ Symbol sourcePos text

bracketed :: P.SourcePos -> Parser Form
bracketed sourcePos = do
  closeBracket <- openBracket
  forms' <- forms
  bracket <- closeBracket P.<|> onEOFFailExpectingMore
  pure $ Bracketed sourcePos bracket forms'
 where
  openBracket :: Parser (Parser Bracket)
  openBracket =
    P.choice . flip map brackets $ \(open, close, bracket) -> do
      _ <- P.Char.char open
      P.Char.space
      pure do
        _ <- P.Char.char close
        pure bracket
  brackets =
    [ ('(', ')', Parens)
    , ('[', ']', SquareBracket)
    , ('{', '}', CurlyBracket)
    ]

comment :: P.SourcePos -> Parser Form
comment sourcePos = do
  _ <- P.lookAhead $ P.Char.string "#"
  text <- P.takeWhileP Nothing (/= '\n')
  pure $ Comment sourcePos text

onEOFFailExpectingMore :: Parser a
onEOFFailExpectingMore =
  P.eof
    *> ( P.fancyFailure . Set.singleton . P.ErrorCustom $
          ExpectedMultilineInput
       )

signed :: (Num a) => Parser a -> Parser a
signed p =
  P.choice
    [ P.try (sign <*> p)
    , p
    ]
 where
  sign = (id <$ P.Char.char '+') P.<|> (negate <$ P.Char.char '-')
