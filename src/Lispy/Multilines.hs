{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lispy.Multilines where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Lispy.Parser (Error)
import UncertainGantt.Script.Types (MoreInputExpected (ExpectedMultilineInput))
import Data.Functor ((<&>))

type Expected = [Char]

get :: (Monoid a) => (Text -> Either (error, Maybe MoreInputExpected) a) -> IO (Either error a)
get parse = go "" mempty
 where
  go previousLines acc = do
    line <- Text.IO.getLine
    let lines = previousLines <> line
    case parse lines of
      Left (_, Just ExpectedMultilineInput) ->
        go (lines <> "\n") acc
      Left (error, _) -> pure (Left error)
      Right value -> pure . Right $ acc <> value
