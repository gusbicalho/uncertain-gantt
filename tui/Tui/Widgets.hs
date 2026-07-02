{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- | Small reusable vty widgets: key events and a form with completion.
module Tui.Widgets (
  Vty,
  keyEv,
  FormField (..),
  FormResult (..),
  form,
) where

import Control.Monad (forM, join)
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Zipper qualified as TZ
import Graphics.Vty qualified as V
import Reflex
import Reflex.Vty

-- | The constraints all our widgets run under (inside @initManager_@).
type Vty t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , MonadNodeId m
  , PostBuild t m
  , HasInput t m
  , HasFocus t m
  , HasFocusReader t m
  , HasLayout t m
  , HasTheme t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  )

-- | A unit event for one specific key + modifier combination.
keyEv :: (Monad m, Reflex t, HasInput t m) => V.Key -> [V.Modifier] -> m (Event t ())
keyEv k mods = fmap (() <$) (keyCombo (k, mods))

data FormField = FormField
  { fieldLabel :: Text
  , fieldInitial :: Text
  , fieldCompletions :: [Text]
  {- ^ Candidates offered while the field is focused; empty for free-form
  fields. Completion applies to the segment after the last comma, so
  list-valued fields complete one item at a time.
  -}
  }

data FormResult t = FormResult
  { formValues :: Dynamic t [Text]
  , formSubmit :: Event t ()
  , formCancel :: Event t ()
  }

{- | A column of labeled single-line text fields. Tab/Shift-Tab move between
fields, Enter submits, Esc cancels. Candidates for the focused field are
shown below the fields; C-n/C-p fill the field with the next/previous
match. The error text (from failed submits) is displayed at the bottom.
-}
form :: (Vty t m) => Text -> Dynamic t Text -> [FormField] -> m (FormResult t)
form title errorDyn fields = do
  fieldResults <- col $ do
    grout (fixed 1) $ text (pure title)
    grout (fixed 1) blank
    fieldResults <- forM fields $ \field ->
      tile' (fixed 1) $ row $ do
        grout (fixed 16) $ text (pure (fieldLabel field <> ":"))
        grout flex $ filterKeys editingKey $ completingInput field
    grout (fixed 1) blank
    focusDyn <- focusedId
    let suggestionsByField = [(fid, suggestionsDyn) | (fid, (_, suggestionsDyn)) <- fieldResults]
        suggestionsLineDyn = join $ ffor focusDyn $ \mbFid ->
          maybe (pure "") snd (mbFid >>= \fid -> (,) fid <$> lookup fid suggestionsByField)
    grout (fixed 1) $ row $ do
      grout (fixed 16) blank
      grout flex $ text (current suggestionsLineDyn)
    grout (fixed 1) $ text (pure "[Enter] save   [Esc] cancel   [Tab] next field   [C-n/C-p] complete")
    grout (fixed 1) $ text (current (errorLine <$> errorDyn))
    pure fieldResults
  let values = fst . snd <$> fieldResults
  tabNavigation
  postBuild <- getPostBuild
  -- Focus the first field by id: a Refocus_Shift at postBuild would sample
  -- the focus set before this form's fields are registered and do nothing.
  case fieldResults of
    (firstFieldId, _) : _ -> requestFocus $ Refocus_Id firstFieldId <$ postBuild
    [] -> pure ()
  submit <- keyEv V.KEnter []
  cancel <- keyEv V.KEsc []
  pure
    FormResult
      { formValues = distributeListOverDyn values
      , formSubmit = submit
      , formCancel = cancel
      }
 where
  errorLine err = if Text.null err then "" else "! " <> err
  editingKey (k, _) = k `notElem` [V.KEnter, V.KEsc, V.KChar '\t', V.KBackTab]

{- | A 'textInput' with completion over a fixed candidate list. Returns the
field's value and the suggestions line to show while it is focused.

C-n/C-p replace the current segment with the next/previous candidate
matching the prefix the user typed (frozen while cycling, vim-style);
any other key resets the cycle.
-}
completingInput :: (Vty t m) => FormField -> m (Dynamic t Text, Dynamic t Text)
completingInput field = do
  nextEv <- keyEv (V.KChar 'n') [V.MCtrl]
  prevEv <- keyEv (V.KChar 'p') [V.MCtrl]
  let cycleEv = leftmost [1 <$ nextEv, (-1 :: Int) <$ prevEv]
  inputEv <- input
  let resetEv = fforMaybe inputEv $ \case
        V.EvKey k mods
          | (k, mods) /= (V.KChar 'n', [V.MCtrl])
          , (k, mods) /= (V.KChar 'p', [V.MCtrl]) ->
              Just ()
        _ -> Nothing
  rec textField <-
        textInput
          def
            { _textInputConfig_initialValue = TZ.fromText (fieldInitial field)
            , _textInputConfig_modify = (\newText _zipper -> TZ.fromText newText) <$> fillEv
            }
      let valueDyn = _textInput_value textField
          stepEv =
            attach (current valueDyn) $
              leftmost [Left <$> cycleEv, Right () <$ resetEv]
      (cycleStateDyn, fillEv) <- mapAccumMaybe step Nothing stepEv
  pure (valueDyn, zipDynWith suggestionsLine cycleStateDyn valueDyn)
 where
  step _ (_, Right ()) = (Just Nothing, Nothing)
  step cycleState (value, Left delta) =
    let (before, segment) = splitLastSegment value
        prefix = maybe segment fst cycleState
     in case matchesFor prefix of
          [] -> (Nothing, Nothing)
          matches ->
            let i = case cycleState of
                  Just (_, j) -> (j + delta) `mod` length matches
                  Nothing -> if delta > 0 then 0 else length matches - 1
             in (Just (Just (prefix, i)), Just (before <> matches !! i))
  suggestionsLine cycleState value =
    let (_, segment) = splitLastSegment value
        prefix = maybe segment fst cycleState
        selected = snd <$> cycleState
     in case matchesFor prefix of
          [] -> ""
          [onlyMatch] | onlyMatch == segment -> ""
          matches ->
            Text.intercalate "  " $
              zipWith
                (\i candidate -> if Just i == selected then "[" <> candidate <> "]" else candidate)
                [0 ..]
                matches
  matchesFor prefix =
    filter
      (Text.isPrefixOf (Text.toLower (Text.strip prefix)) . Text.toLower)
      (fieldCompletions field)

-- | Split off the (whitespace-stripped) segment after the last comma.
splitLastSegment :: Text -> (Text, Text)
splitLastSegment value =
  let (before, rawSegment) = Text.breakOnEnd "," value
      (spaces, segment) = Text.span (== ' ') rawSegment
   in (before <> spaces, segment)
