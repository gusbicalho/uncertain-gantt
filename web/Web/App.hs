{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The whole browser UI as one 'HyperView': a 'Screen' (which sub-view is
showing) plus the editable 'Doc', held in a single server-side 'TVar' and
swapped via action-triggered re-renders. There is no per-request session
store in Hyperbole and 'update' is dispatched generically by the library
with no way to close a per-page parameter into it, so a top-level 'TVar'
is the only place this app's state can live between requests.

Rendering lives in this module too (not split out): the 'render' function
needs the 'Action' constructors and 'AppState'/'Screen' accessors, and
'update' needs 'render' — splitting them would make the two modules import
each other.
-}
module Web.App (
  App (..),
  initGlobalState,
  page,
) where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS8
import Data.List (find)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Editor.Doc (
  Doc,
  DocOp (OpDelete, OpInsert, OpReplace),
  Element (ElemAlias, ElemResource, ElemTask),
  FormSpec (formFields, formParse, formTitle),
  applyOp,
  editSpec,
  newAliasSpec,
  newResourceSpec,
  newTaskSpec,
 )
import Editor.Estimate (Report (reportRuns, reportSamples, reportTasksExcluded, reportTasksIncluded), defaultRuns, runReport)
import Editor.FormField (FormField (fieldCompletions, fieldInitial, fieldLabel))
import Editor.Persistence (AppConfig (appInitialDoc, appInitialNote, appSave, appTitle))
import Editor.View qualified as EditorView
import Effectful (IOE, liftIO)
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Numeric (showFFloat)
import System.IO.Unsafe (unsafePerformIO)
import UncertainGantt.Sim.Stats qualified as Stats
import Web.Hyperbole
import Web.Hyperbole.Effect.Request (formBody)

data App = App
  deriving stock (Generic)
  deriving anyclass (ViewId)

{- | Which screen is showing, and (for a form) where to return and which
'Doc' index (if any) it is editing.
-}
data Screen
  = STasks
  | SResources
  | SDurations
  | SForm Screen FormSpec (Maybe Int)

data AppState = AppState
  { stDoc :: Doc
  , stScreen :: Screen
  , stArmedDelete :: Maybe Int
  , stReport :: Maybe (Either Text Report)
  , stDirty :: Bool
  , stTitle :: Text
  , stNote :: Maybe Text
  , stStatus :: Maybe Text
  , stFormError :: Maybe Text
  , stSaveDoc :: Doc -> IO Text
  }

{-# NOINLINE globalState #-}
globalState :: TVar AppState
globalState =
  unsafePerformIO $
    newTVarIO
      AppState
        { stDoc = []
        , stScreen = STasks
        , stArmedDelete = Nothing
        , stReport = Nothing
        , stDirty = False
        , stTitle = ""
        , stNote = Nothing
        , stStatus = Nothing
        , stFormError = Nothing
        , stSaveDoc = \_ -> pure ""
        }

{- | Populate the global state from a loaded project; call once from @main@
before starting the server.
-}
initGlobalState :: AppConfig -> IO ()
initGlobalState cfg =
  atomically . writeTVar globalState $
    AppState
      { stDoc = appInitialDoc cfg
      , stScreen = STasks
      , stArmedDelete = Nothing
      , stReport = Nothing
      , stDirty = False
      , stTitle = appTitle cfg
      , stNote = appInitialNote cfg
      , stStatus = Nothing
      , stFormError = Nothing
      , stSaveDoc = appSave cfg
      }

page :: (IOE :> es) => Page es '[App]
page = do
  st <- liftIO (readTVarIO globalState)
  pure $ hyper App (render st)

instance (IOE :> es) => HyperView App es where
  data Action App
    = OpenAdd
    | OpenEdit Int
    | RequestDelete Int
    | ConfirmDelete Int
    | CancelDelete
    | OpenResources
    | OpenDurations
    | Back
    | SubmitForm
    | CancelForm
    | RunEstimate
    | SaveDoc
    deriving stock (Generic)
    deriving anyclass (ViewAction)

  update SubmitForm = do
    submittedForm <- formBody
    st <- liftIO $ atomically $ do
      st0 <- readTVar globalState
      let st1 = handleSubmit submittedForm st0
      writeTVar globalState st1
      pure st1
    pure (render st)
  update RunEstimate = do
    st0 <- liftIO (readTVarIO globalState)
    result <- liftIO (runReport defaultRuns (stDoc st0))
    st <- liftIO $ atomically $ do
      let st1 = (disarm st0){stReport = Just result}
      writeTVar globalState st1
      pure st1
    pure (render st)
  update SaveDoc = do
    st0 <- liftIO (readTVarIO globalState)
    msg <- liftIO (stSaveDoc st0 (stDoc st0))
    st <- liftIO $ atomically $ do
      let st1 = (disarm st0){stDirty = False, stStatus = Just msg}
      writeTVar globalState st1
      pure st1
    pure (render st)
  update action = do
    st <- liftIO $ atomically $ do
      st0 <- readTVar globalState
      let st1 = applyAction action st0
      writeTVar globalState st1
      pure st1
    pure (render st)

{- | Reset the guarded-delete "armed" flag; every action other than the
delete actions themselves does this first (mirrors the TUI's
disarm-on-any-other-activity rule).
-}
disarm :: AppState -> AppState
disarm st = st{stArmedDelete = Nothing}

applyAction :: Action App -> AppState -> AppState
applyAction OpenAdd st =
  case newSpecFor (stScreen st) (stDoc st) of
    Just spec -> (disarm st){stScreen = SForm (stScreen st) spec Nothing, stFormError = Nothing}
    Nothing -> disarm st
applyAction (OpenEdit i) st =
  case drop i (stDoc st) of
    (element : _) ->
      (disarm st)
        { stScreen = SForm (stScreen st) (editSpec (stDoc st) element) (Just i)
        , stFormError = Nothing
        }
    [] -> disarm st
applyAction (RequestDelete i) st
  | null (elementUsers (stDoc st) i) = applyAction (ConfirmDelete i) st
  | otherwise = (disarm st){stArmedDelete = Just i}
applyAction (ConfirmDelete i) st =
  (disarm st){stDoc = applyOp (OpDelete i) (stDoc st), stDirty = True}
applyAction CancelDelete st = disarm st
applyAction OpenResources st = (disarm st){stScreen = SResources}
applyAction OpenDurations st = (disarm st){stScreen = SDurations}
applyAction Back st = (disarm st){stScreen = backTo (stScreen st), stFormError = Nothing}
applyAction CancelForm st = (disarm st){stScreen = backTo (stScreen st), stFormError = Nothing}
applyAction SubmitForm st = st -- handled specially in 'update'
applyAction RunEstimate st = st -- handled specially in 'update'
applyAction SaveDoc st = st -- handled specially in 'update'

backTo :: Screen -> Screen
backTo (SForm returnTo _ _) = returnTo
backTo _ = STasks

newSpecFor :: Screen -> Doc -> Maybe FormSpec
newSpecFor STasks doc = Just (newTaskSpec doc)
newSpecFor SResources _ = Just newResourceSpec
newSpecFor SDurations _ = Just newAliasSpec
newSpecFor (SForm _ _ _) _ = Nothing

{- | Names of tasks that reference the element at this 'Doc' index (task
dependents for a task, users for a resource or duration alias). Empty
means it's safe to delete without confirmation.
-}
elementUsers :: Doc -> Int -> [Text]
elementUsers doc i = case drop i doc of
  (ElemResource{} : _) ->
    maybe [] EditorView.resourceRowUsedBy (find ((== i) . EditorView.resourceRowIndex) (EditorView.resourceRows doc))
  (ElemAlias{} : _) ->
    maybe [] EditorView.durationRowUsedBy (find ((== i) . EditorView.durationRowIndex) (EditorView.durationRows doc))
  (ElemTask{} : _) ->
    maybe [] EditorView.taskRowDependents (find ((== i) . EditorView.taskRowIndex) (EditorView.taskRows doc))
  [] -> []

handleSubmit :: Form -> AppState -> AppState
handleSubmit submittedForm st = case stScreen st of
  SForm returnTo spec mbIndex ->
    let values = fieldValues (length (formFields spec)) submittedForm
     in case formParse spec values of
          Left err -> st{stFormError = Just err}
          Right element ->
            let op = maybe (OpInsert element) (`OpReplace` element) mbIndex
             in (disarm st)
                  { stDoc = applyOp op (stDoc st)
                  , stScreen = returnTo
                  , stDirty = True
                  , stFormError = Nothing
                  }
  _ -> st

fieldValues :: Int -> Form -> [Text]
fieldValues n submittedForm =
  [ either (const "") id (parseField @Text (fieldKey i) submittedForm)
  | i <- [0 .. n - 1]
  ]

fieldKey :: Int -> BS8.ByteString
fieldKey i = BS8.pack ("field-" <> show i)

------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------

render :: AppState -> View App ()
render st = do
  styles
  el @ att "class" "app" $ do
    header st
    case stScreen st of
      STasks -> tasksScreen st
      SResources -> panelScreen st "Resources" "Add resource" resourcePanel
      SDurations -> panelScreen st "Durations" "Add duration alias" durationPanel
      SForm _ spec _ -> formScreen spec (stFormError st)

header :: AppState -> View App ()
header st = el @ att "class" "header" $ do
  el @ att "class" "title" $ text (stTitle st)
  maybe none (\note -> el @ att "class" "note" $ text note) (stNote st)
  maybe none (\msg -> el @ att "class" "status" $ text msg) (stStatus st)
  when (stDirty st) $ el @ att "class" "dirty" $ text "Unsaved changes"
  button SaveDoc (text "Save") @ att "class" "btn btn-primary"

tasksScreen :: AppState -> View App ()
tasksScreen st = el @ att "class" "columns" $ do
  el @ att "class" "column" $ do
    vocabularyStrip (stDoc st)
    taskTable (stArmedDelete st) (stDoc st)
    button OpenAdd (text "Add task") @ att "class" "btn"
  el @ att "class" "column" $ estimatePanel (stReport st)

vocabularyStrip :: Doc -> View App ()
vocabularyStrip doc = el @ att "class" "vocab" $ do
  el @ att "class" "vocab-line" $ do
    text ("Resources: " <> resourceSummary <> "  ")
    button OpenResources (text "Manage") @ att "class" "btn-link"
  el @ att "class" "vocab-line" $ do
    text ("Durations: " <> durationSummary <> "  ")
    button OpenDurations (text "Manage") @ att "class" "btn-link"
 where
  resourceSummary = case EditorView.resourceRows doc of
    [] -> "(none)"
    rows -> Text.intercalate " · " [EditorView.resourceRowName r <> " ×" <> showT (EditorView.resourceRowCapacity r) | r <- rows]
  durationSummary = case EditorView.durationRows doc of
    [] -> "(none)"
    rows -> Text.intercalate " · " [EditorView.durationRowName r <> " " <> EditorView.durationRowDefinition r | r <- rows]

taskTable :: Maybe Int -> Doc -> View App ()
taskTable armed doc = case EditorView.taskRows doc of
  [] -> el @ att "class" "empty" $ text "No tasks yet — Add task to start."
  rows ->
    table rows $ do
      tcol (th (text "Task")) $ \r -> td @ att "class" "cell-task" $ taskCell r
      tcol (th (text "Resource")) $ \r -> td $ text (EditorView.taskRowResource r)
      tcol (th (text "Duration")) $ \r -> td $ text (EditorView.taskRowDuration r)
      tcol (th (text "After")) $ \r -> td $ text (afterText r)
      tcol (th none) $ \r -> td $ rowActions armed (EditorView.taskRowIndex r)
 where
  afterText r = case EditorView.taskRowAfter r of
    [] -> "—"
    after -> Text.intercalate ", " after
  taskCell r = do
    el @ att "class" ("task-name depth-" <> showT (EditorView.taskRowDepth r)) $
      text (EditorView.taskRowName r)
    when (not (Text.null (EditorView.taskRowDescription r))) $
      el @ att "class" "task-description" $
        text (EditorView.taskRowDescription r)
    when (not (null (EditorView.taskRowIssues r))) $
      el @ att "class" "task-issues" $
        text ("! " <> Text.intercalate "  ·  " (EditorView.taskRowIssues r))

resourcePanel :: Maybe Int -> Doc -> View App ()
resourcePanel armed doc = case EditorView.resourceRows doc of
  [] -> el @ att "class" "empty" $ text "No resources yet — Add resource to start."
  rows ->
    table rows $ do
      tcol (th (text "Name")) $ \r -> td $ text (EditorView.resourceRowName r)
      tcol (th (text "Capacity")) $ \r -> td $ text (showT (EditorView.resourceRowCapacity r))
      tcol (th (text "Used by")) $ \r -> td $ text (usedByText (EditorView.resourceRowUsedBy r))
      tcol (th none) $ \r -> td $ rowActions armed (EditorView.resourceRowIndex r)

durationPanel :: Maybe Int -> Doc -> View App ()
durationPanel armed doc = case EditorView.durationRows doc of
  [] -> el @ att "class" "empty" $ text "No duration aliases yet — Add duration to start."
  rows ->
    table rows $ do
      tcol (th (text "Name")) $ \r -> td $ text (EditorView.durationRowName r)
      tcol (th (text "Definition")) $ \r -> td $ text (EditorView.durationRowDefinition r)
      tcol (th (text "Used by")) $ \r -> td $ text (usedByText (EditorView.durationRowUsedBy r))
      tcol (th none) $ \r -> td $ rowActions armed (EditorView.durationRowIndex r)

usedByText :: [Text] -> Text
usedByText [] = "unused"
usedByText ts = Text.intercalate ", " ts

rowActions :: Maybe Int -> Int -> View App ()
rowActions armed i = do
  button (OpenEdit i) (text "Edit") @ att "class" "btn-link"
  if armed == Just i
    then button (ConfirmDelete i) (text "Confirm delete?") @ att "class" "btn-link btn-danger"
    else button (RequestDelete i) (text "Delete") @ att "class" "btn-link"

panelScreen :: AppState -> Text -> Text -> (Maybe Int -> Doc -> View App ()) -> View App ()
panelScreen st heading addLabel body = el @ att "class" "column" $ do
  el @ att "class" "panel-heading" $ text heading
  body (stArmedDelete st) (stDoc st)
  button OpenAdd (text addLabel) @ att "class" "btn"
  button Back (text "Back") @ att "class" "btn"

formScreen :: FormSpec -> Maybe Text -> View App ()
formScreen spec err = el @ att "class" "column" $ do
  el @ att "class" "panel-heading" $ text (formTitle spec)
  maybe none (\msg -> el @ att "class" "form-error" $ text ("! " <> msg)) err
  form SubmitForm @ att "class" "form" $ do
    mapM_ (uncurry renderField) (zip [0 :: Int ..] (formFields spec))
    submit (text "Save") @ att "class" "btn btn-primary"
  button CancelForm (text "Cancel") @ att "class" "btn"

renderField :: Int -> FormField -> View (FormFields App) ()
renderField i f = field (fieldNameFor i) $ do
  label @ att "class" "field-label" $ text (fieldLabel f)
  input TextInput @ value (fieldInitial f) . att "list" dlId @ att "class" "field-input"
  tag "datalist" @ att "id" dlId $
    mapM_ (\c -> tag "option" @ att "value" c $ none) (fieldCompletions f)
 where
  dlId = "dl-" <> showT i

fieldNameFor :: Int -> FieldName Text
fieldNameFor i = fromString ("field-" <> show i)

estimatePanel :: Maybe (Either Text Report) -> View App ()
estimatePanel mbReport = el @ att "class" "estimate" $ do
  el @ att "class" "panel-heading" $ text "Estimate"
  button RunEstimate (text "Run estimate") @ att "class" "btn"
  case mbReport of
    Nothing -> el @ att "class" "empty" $ text "No estimate yet."
    Just (Left err) -> el @ att "class" "form-error" $ text ("! " <> err)
    Just (Right report) -> reportView report

reportView :: Report -> View App ()
reportView report = do
  el @ att "class" "estimate-summary" $
    text
      ( showT (reportRuns report)
          <> " simulation runs"
          <> if reportTasksExcluded report > 0
            then " (" <> showT (reportTasksIncluded report) <> " tasks; " <> showT (reportTasksExcluded report) <> " excluded)"
            else ""
      )
  el @ att "class" "estimate-summary" $ text ("mean " <> f1 (Stats.weightedAverage samples))
  el @ att "class" "estimate-quantiles" $
    mapM_
      (\p -> el @ att "class" "quantile" $ text ("p" <> showT p <> " " <> f1 (Stats.quantile p 100 samples)))
      ([5, 25, 50, 75, 90, 95] :: [Word])
  histogram (Stats.histogram 10 (Stats.p99range samples) samples)
 where
  samples = reportSamples report

histogram :: [Stats.HistogramEntry] -> View App ()
histogram entries = el @ att "class" "hist" $ mapM_ bucket entries
 where
  bucket Stats.HistogramEntry{Stats.entryLowerEnd, Stats.entryWeight, Stats.entryFraction} =
    el @ att "class" "hist-row" $ do
      el @ att "class" "hist-bound" $ text (boundText entryLowerEnd)
      el @ att "class" "hist-bar-track" $
        el @ att "style" ("width:" <> f1 (entryWeight * 100) <> "%") @ att "class" "hist-bar-fill" $
          none
      el @ att "class" "hist-pct" $ text (f1 (entryFraction * 100) <> "%")
  boundText x = if isInfinite x then "below" else f1 x

f1 :: Double -> Text
f1 x = Text.pack (showFFloat (Just 1) x "")

showT :: (Show a) => a -> Text
showT = Text.pack . show

styles :: View App ()
styles =
  style
    "\
    \.app { font-family: system-ui, -apple-system, \"Segoe UI\", sans-serif; color: #0b0b0b; max-width: 1200px; margin: 0 auto; padding: 16px; }\
    \.header { display: flex; align-items: center; gap: 12px; margin-bottom: 16px; }\
    \.title { font-weight: 600; }\
    \.note, .status { color: #52514e; }\
    \.dirty { color: #d03b3b; }\
    \.columns { display: flex; gap: 24px; align-items: flex-start; }\
    \.column { flex: 1; min-width: 0; }\
    \.vocab-line { color: #52514e; margin-bottom: 4px; }\
    \.panel-heading { font-weight: 600; margin: 8px 0; }\
    \table { border-collapse: collapse; width: 100%; margin-bottom: 8px; }\
    \th { text-align: left; color: #898781; font-weight: 600; border-bottom: 1px solid #c3c2b7; padding: 4px 8px 4px 0; }\
    \td { padding: 4px 8px 4px 0; border-bottom: 1px solid #e1e0d9; vertical-align: top; }\
    \.task-name.depth-1 { padding-left: 16px; }\
    \.task-name.depth-2 { padding-left: 32px; }\
    \.task-name.depth-3 { padding-left: 48px; }\
    \.task-name.depth-4 { padding-left: 64px; }\
    \.task-description, .task-issues { color: #52514e; font-size: 0.9em; }\
    \.task-issues { color: #d03b3b; }\
    \.empty { color: #898781; padding: 8px 0; }\
    \.btn { background: #2a78d6; color: #fff; border: none; border-radius: 4px; padding: 6px 12px; cursor: pointer; margin: 4px 4px 4px 0; }\
    \.btn-primary { background: #2a78d6; }\
    \.btn-link { background: none; color: #2a78d6; border: none; cursor: pointer; padding: 2px 6px; }\
    \.btn-danger { color: #d03b3b; }\
    \.form-error { color: #d03b3b; margin: 4px 0; }\
    \.form { display: flex; flex-direction: column; gap: 8px; max-width: 480px; }\
    \.field-label { display: block; color: #52514e; margin-bottom: 2px; }\
    \.field-input { width: 100%; padding: 6px 8px; border: 1px solid #c3c2b7; border-radius: 4px; }\
    \.estimate { border-left: 1px solid #e1e0d9; padding-left: 24px; }\
    \.estimate-summary { margin-bottom: 4px; }\
    \.estimate-quantiles { display: flex; flex-wrap: wrap; gap: 12px; margin: 8px 0; color: #52514e; }\
    \.hist { display: flex; flex-direction: column; gap: 2px; margin-top: 8px; }\
    \.hist-row { display: grid; grid-template-columns: 64px 1fr 48px; align-items: center; gap: 8px; }\
    \.hist-bound { color: #52514e; text-align: right; font-variant-numeric: tabular-nums; }\
    \.hist-bar-track { background: #cde2fb; border-radius: 4px; height: 18px; overflow: hidden; }\
    \.hist-bar-fill { background: #2a78d6; height: 100%; border-radius: 0 4px 4px 0; }\
    \.hist-pct { color: #52514e; font-variant-numeric: tabular-nums; }\
    \@media (prefers-color-scheme: dark) {\
    \  .app { background: #1a1a19; color: #ffffff; }\
    \  .note, .status, .vocab-line, .task-description, .estimate-quantiles, .hist-bound, .hist-pct { color: #c3c2b7; }\
    \  th { color: #898781; border-bottom-color: #383835; }\
    \  td { border-bottom-color: #2c2c2a; }\
    \  .empty { color: #898781; }\
    \  .btn, .btn-primary, .hist-bar-fill { background: #3987e5; }\
    \  .btn-link { color: #3987e5; }\
    \  .field-input { background: #1a1a19; color: #fff; border-color: #383835; }\
    \  .estimate { border-left-color: #2c2c2a; }\
    \  .hist-bar-track { background: #184f95; }\
    \}"
