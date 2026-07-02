{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Rendering statements back into the script syntax accepted by
"UncertainGantt.Script.Parser".
-}
module UncertainGantt.Script.Render (
  renderDeclarations,
  renderStatement,
  renderDuration,
  renderName,
) where

import Data.Char qualified as Char
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Symbolize (Symbol)
import UncertainGantt.Script.ToText (ToText (toText), showText)
import UncertainGantt.Script.Types (
  DurationD (LogNormalD, NormalD, UniformD),
  ResourceDescription (ResourceDescription),
  Statement (AddResource, AddTask, DurationAliasDeclaration),
  TaskDescription (TaskDescription),
  unDurationAlias,
  unResource,
 )
import UncertainGantt.Task (unTaskName)

{- | Render the declarative subset of a script: resources, duration aliases
and tasks. Statements that merely print or run simulations have no
declarative content and are skipped.

For any statement @renderStatement@ accepts, feeding the output back to
'UncertainGantt.Script.Parser.parseScript' yields the same statement.
-}
renderDeclarations :: [Statement] -> Text
renderDeclarations = Text.concat . Maybe.mapMaybe renderStatement

-- | Render a single declarative statement, 'Nothing' for the others.
renderStatement :: Statement -> Maybe Text
renderStatement = \case
  AddResource (ResourceDescription resource amount) ->
    Just $ "resource " <> renderName (unResource resource) <> " " <> showText amount <> "\n"
  DurationAliasDeclaration alias duration ->
    Just $ "duration " <> renderName (unDurationAlias alias) <> " " <> renderDuration duration <> "\n"
  AddTask (TaskDescription taskName description resource duration dependencies) ->
    Just . Text.concat $
      [ "task " <> renderName (unTaskName taskName) <> "\n"
      , "  " <> renderName (unResource resource) <> "\n"
      , "  " <> either (renderName . unDurationAlias) renderDuration duration <> "\n"
      ]
        <> [ "  depends on " <> Text.intercalate ", " (renderName . unTaskName <$> dependencies) <> "\n"
           | not (null dependencies)
           ]
        <> [ "  " <> description <> "\n"
           | not (Text.null description)
           ]
  _ -> Nothing

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
