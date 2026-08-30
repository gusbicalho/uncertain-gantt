{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Rendering expression-level pieces of the definition language back into
their concrete syntax. For any output of these functions, the matching
parser in "UncertainGantt.Lang.Parser" yields the original value.
-}
module UncertainGantt.Lang.Render (
  renderDuration,
  renderName,
) where

import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Symbolize (Symbol)
import UncertainGantt.Lang.Types (DurationD (LogNormalD, NormalD, UniformD))
import UncertainGantt.ToText (ToText (toText), showText)

renderDuration :: DurationD -> Text
renderDuration = \case
  UniformD from to -> "uniform " <> showText from <> " " <> showText to
  NormalD average stddev -> "normal " <> showText average <> " " <> showText stddev
  LogNormalD median stddev -> "logNormal " <> showText median <> " " <> showText stddev

-- | Names render bare when purely alphanumeric, quoted otherwise.
renderName :: Symbol -> Text
renderName symbol
  | not (Text.null name) && Text.all Char.isAlphaNum name = name
  | otherwise = showText (Text.unpack name)
 where
  name = toText symbol
