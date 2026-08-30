{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Rendering statements back into the script syntax accepted by
"UncertainGantt.Script.Parser". The expression-level pieces live in
"UncertainGantt.Lang.Render".
-}
module UncertainGantt.Script.Render (
  renderDeclarations,
  renderStatement,
) where

import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import UncertainGantt.Lang.Render (renderDuration, renderName)
import UncertainGantt.Lang.Types (
  ResourceDescription (ResourceDescription),
  TaskDescription (TaskDescription),
  unDurationAlias,
  unResource,
 )
import UncertainGantt.Script.Types (Statement (AddResource, AddTask, DurationAliasDeclaration))
import UncertainGantt.Task (unTaskName)
import UncertainGantt.ToText (showText)

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
