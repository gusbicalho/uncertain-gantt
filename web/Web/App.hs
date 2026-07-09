{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The browser editor, one page of three HyperViews (see web/DESIGN.md):

* 'Header' — title, dirty flag, status toast, Undo, Save.
* 'TaskTable' — the vocabulary sections and the task table. Rows edit in
  place (click a cell → the row becomes a form with that field focused);
  the last row of each section is a permanent quick-add form. Every
  committed change pushes fresh renders of the other two views.
* 'EstimatePanel' — the live estimate. Whenever the state is stale and
  auto-estimate is on, the panel renders an @onLoad Recalc@ element, so
  the client immediately asks for a recalculation; 'Recalc' blocks on
  the simulation (each action runs in its own thread, and a newer action
  on the same view cancels the running one, so a burst of edits simply
  restarts the run). An epoch counter discards results that raced with
  a newer edit — the stale state then re-arms the @onLoad@, making the
  loop self-correcting.

State is one module-level 'TVar': Hyperbole has no per-request session
store suitable for a whole 'Doc', and 'update' is dispatched by the
library with no way to pass a handle in (single-file editor, one
document per process, so global state is the honest shape).
-}
module Web.App (
  Header (..),
  TaskTable (..),
  EstimatePanel (..),
  initGlobalState,
  page,
) where

import Control.Monad (unless, when)
import Data.ByteString.Char8 qualified as BS8
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Editor.Doc (
  Doc,
  DocOp (OpDelete, OpInsert, OpReplace),
  Element (ElemAlias, ElemResource, ElemTask),
  FormSpec (formFields, formParse),
  applyOp,
  lenientEditSpec,
  lenientResourceSpec,
  lenientTaskSpec,
  newAliasSpec,
 )
import Editor.Estimate (Report (reportRuns, reportSamples, reportTasksExcluded, reportTasksIncluded), defaultRuns, runReport)
import Editor.FormField (FormField (fieldCompletions, fieldInitial, fieldLabel))
import Editor.Persistence (AppConfig (appInitialDoc, appInitialNote, appSave, appTitle))
import Editor.View qualified as EditorView
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Reader.Dynamic (Reader)
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Numeric (showFFloat)
import System.IO.Unsafe (unsafePerformIO)
import UncertainGantt qualified as UG
import UncertainGantt.Lang.Types (
  ResourceDescription (ResourceDescription),
  TaskDescription (TaskDescription),
  unDurationAlias,
  unResource,
 )
import UncertainGantt.Sim.Stats qualified as Stats
import UncertainGantt.ToText (toText)
import Web.Hyperbole
import Web.Hyperbole.Effect.Request (formBody)
import Web.Hyperbole.View.ViewAction (encodeAction)

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

data AppState = AppState
  { stDoc :: Doc
  , stUndo :: [Doc]
  -- ^ Snapshots before each change, newest first (capped).
  , stEpoch :: Int
  -- ^ Bumped on every doc change; guards against stale estimate results.
  , stEditing :: Maybe (Int, Int)
  -- ^ Row in edit mode: doc index and the field to focus.
  , stShowResources :: Bool
  , stShowDurations :: Bool
  , stAliasDraft :: Maybe Text
  -- ^ Prefilled name for the duration quick-add (create-from-use).
  , stReport :: Maybe (Either Text Report)
  , stPrevReport :: Maybe Report
  -- ^ The previous successful report, for the delta line.
  , stEstimating :: Bool
  , stAuto :: Bool
  , stStale :: Bool
  -- ^ Doc changed since the report shown was computed.
  , stDirty :: Bool
  , stTitle :: Text
  , stNote :: Maybe Text
  , stStatus :: Maybe Text
  , stSaveDoc :: Doc -> IO Text
  }

undoLimit :: Int
undoLimit = 100

{-# NOINLINE globalState #-}
globalState :: TVar AppState
globalState =
  unsafePerformIO $
    newTVarIO
      AppState
        { stDoc = []
        , stUndo = []
        , stEpoch = 0
        , stEditing = Nothing
        , stShowResources = False
        , stShowDurations = False
        , stAliasDraft = Nothing
        , stReport = Nothing
        , stPrevReport = Nothing
        , stEstimating = False
        , stAuto = True
        , stStale = True
        , stDirty = False
        , stTitle = ""
        , stNote = Nothing
        , stStatus = Nothing
        , stSaveDoc = \_ -> pure ""
        }

{- | Populate the global state from a loaded project; call once from @main@
before starting the server.
-}
initGlobalState :: AppConfig -> IO ()
initGlobalState cfg =
  atomically $ do
    st <- readTVar globalState
    writeTVar globalState $
      st
        { stDoc = appInitialDoc cfg
        , stTitle = appTitle cfg
        , stNote = appInitialNote cfg
        , stSaveDoc = appSave cfg
        }

modifyState :: (IOE :> es) => (AppState -> AppState) -> Eff es AppState
modifyState f = liftIO . atomically $ do
  st0 <- readTVar globalState
  let st1 = f st0
  writeTVar globalState st1
  pure st1

{- | Commit a document change: push an undo snapshot, bump the epoch (so a
racing estimate result is discarded), mark stale + dirty, leave edit
mode. @extra@ runs on the resulting state (e.g. to focus a new row).
-}
applyDocChange :: (IOE :> es) => Maybe Text -> (Doc -> Doc) -> (AppState -> AppState) -> Eff es AppState
applyDocChange status f extra =
  modifyState $ \st0 ->
    extra
      st0
        { stDoc = f (stDoc st0)
        , stUndo = take undoLimit (stDoc st0 : stUndo st0)
        , stEpoch = stEpoch st0 + 1
        , stEditing = Nothing
        , stAliasDraft = Nothing
        , stStale = True
        , stDirty = True
        , stStatus = status
        }

------------------------------------------------------------------------------
-- Page and views
------------------------------------------------------------------------------

page :: (IOE :> es) => Page es '[Header, TaskTable, EstimatePanel]
page = do
  st <- liftIO (readTVarIO globalState)
  pure $ do
    styles
    el @ att "class" "app" $ do
      hyper Header (headerView st)
      el @ att "class" "columns" $ do
        el @ att "class" "column column-main" $ hyper TaskTable (tableView st plain)
        el @ att "class" "column column-est" $ hyper EstimatePanel (estimatePanelView st)

data Header = Header
  deriving stock (Generic)
  deriving anyclass (ViewId)

instance (IOE :> es) => HyperView Header es where
  data Action Header = SaveDoc | Undo | HeaderRefresh
    deriving stock (Generic)
    deriving anyclass (ViewAction)

  type Require Header = '[TaskTable, EstimatePanel]

  update HeaderRefresh = do
    st <- liftIO (readTVarIO globalState)
    pure (headerView st)
  update SaveDoc = do
    st0 <- liftIO (readTVarIO globalState)
    msg <- liftIO (stSaveDoc st0 (stDoc st0))
    st <- modifyState $ \s -> s{stDirty = False, stStatus = Just msg}
    pure (headerView st)
  update Undo = do
    st <- modifyState $ \st0 -> case stUndo st0 of
      [] -> st0
      (doc : rest) ->
        st0
          { stDoc = doc
          , stUndo = rest
          , stEpoch = stEpoch st0 + 1
          , stEditing = Nothing
          , stAliasDraft = Nothing
          , stStale = True
          , stDirty = True
          , stStatus = Just "Undone"
          }
    trigger TaskTable TableRefresh
    trigger EstimatePanel Refresh
    pure (headerView st)

data TaskTable = TaskTable
  deriving stock (Generic)
  deriving anyclass (ViewId)

instance (IOE :> es) => HyperView TaskTable es where
  data Action TaskTable
    = TableRefresh
    | EditRow Int Int
    | CancelEdit
    | CommitRow Int
    | DeleteRow Int
    | QuickAddTask
    | QuickAddResource
    | QuickAddAlias
    | ToggleResources
    | ToggleDurations
    | DefineResource Text
    | DefineAlias Text
    deriving stock (Generic)
    deriving anyclass (ViewAction)

  type Require TaskTable = '[Header, EstimatePanel]

  update TableRefresh = do
    st <- liftIO (readTVarIO globalState)
    pure (tableView st plain)
  update (EditRow i focus) = do
    st <- modifyState $ \s -> s{stEditing = Just (i, focus)}
    pure (tableView st plain)
  update CancelEdit = do
    st <- modifyState $ \s -> s{stEditing = Nothing, stAliasDraft = Nothing}
    pure (tableView st plain)
  update (CommitRow i) = do
    submitted <- formBody
    st0 <- liftIO (readTVarIO globalState)
    case drop i (stDoc st0) of
      (element : _) -> do
        let spec = lenientEditSpec (stDoc st0) element
            values = fieldValues (length (formFields spec)) submitted
        case formParse spec values of
          Left err -> pure (tableView st0 plain{tmRetry = Just (FormRetry (SlotEdit i) values err)})
          Right element' -> do
            st <- applyDocChange Nothing (applyOp (OpReplace i element')) id
            refreshOthers
            pure (tableView st plain)
      [] -> pure (tableView st0 plain)
  update (DeleteRow i) = do
    st0 <- liftIO (readTVarIO globalState)
    case drop i (stDoc st0) of
      (element : _) -> do
        let status = "Deleted \"" <> elementName element <> "\" — Undo to restore"
        st <- applyDocChange (Just status) (applyOp (OpDelete i)) id
        refreshOthers
        pure (tableView st plain)
      [] -> pure (tableView st0 plain)
  update QuickAddTask = quickAdd SlotQuickTask (\doc -> lenientTaskSpec doc Nothing)
  update QuickAddResource = quickAdd SlotQuickResource (const (lenientResourceSpec Nothing))
  update QuickAddAlias = quickAdd SlotQuickAlias (const newAliasSpec)
  update ToggleResources = do
    st <- modifyState $ \s -> s{stShowResources = not (stShowResources s)}
    pure (tableView st plain)
  update ToggleDurations = do
    st <- modifyState $ \s -> s{stShowDurations = not (stShowDurations s)}
    pure (tableView st plain)
  update (DefineResource name) = do
    let element = ElemResource (ResourceDescription (fromString (Text.unpack name)) 1)
    st <-
      applyDocChange
        (Just ("Defined resource " <> name <> " ×1"))
        (applyOp (OpInsert element))
        (\s -> s{stShowResources = True, stEditing = Just (length (stDoc s) - 1, 1)})
    refreshOthers
    pure (tableView st plain)
  update (DefineAlias name) = do
    st <- modifyState $ \s -> s{stShowDurations = True, stAliasDraft = Just name, stEditing = Nothing}
    pure (tableView st plain)

-- | Handle a quick-add form submission for one of the three sections.
quickAdd ::
  (Hyperbole :> es, IOE :> es) =>
  FormSlot ->
  (Doc -> FormSpec) ->
  Eff (Reader TaskTable : es) (View TaskTable ())
quickAdd slot mkSpec = do
  submitted <- formBody
  st0 <- liftIO (readTVarIO globalState)
  let spec = mkSpec (stDoc st0)
      values = fieldValues (length (formFields spec)) submitted
  case formParse spec values of
    Left err -> pure (tableView st0 plain{tmRetry = Just (FormRetry slot values err)})
    Right element -> do
      st <- applyDocChange Nothing (applyOp (OpInsert element)) id
      refreshOthers
      pure (tableView st plain{tmFocusQuick = Just slot})

{- | After a doc change from the table: have the client re-request the
other two views. This must be 'trigger', not 'pushUpdateTo' — form
submissions arrive over HTTP, where pushes are silently dropped but
triggers ride back as response metadata.
-}
refreshOthers :: (Hyperbole :> es, IOE :> es) => Eff (Reader TaskTable : es) ()
refreshOthers = do
  trigger Header HeaderRefresh
  trigger EstimatePanel Refresh

elementName :: Element -> Text
elementName = \case
  ElemResource (ResourceDescription r _) -> toText (unResource r)
  ElemAlias a _ -> toText (unDurationAlias a)
  ElemTask (TaskDescription n _ _ _ _) -> toText (UG.unTaskName n)

data EstimatePanel = EstimatePanel
  deriving stock (Generic)
  deriving anyclass (ViewId)

instance (IOE :> es) => HyperView EstimatePanel es where
  data Action EstimatePanel = Refresh | Recalc | ToggleAuto
    deriving stock (Generic)
    deriving anyclass (ViewAction)

  -- A newer action cancels the running one server-side; Replace makes the
  -- client keep sending them instead of dropping while one is in flight.
  type Concurrency EstimatePanel = 'Replace

  -- Triggered after every doc change. Arriving on this view, it cancels
  -- any in-flight Recalc — whose snapshot predates the change, so its
  -- result was stale anyway — which is why it must clear stEstimating;
  -- the stale+auto render then re-arms the onLoad loop.
  update Refresh = do
    st <- modifyState $ \s -> s{stEstimating = False}
    pure (estimatePanelView st)
  update Recalc = do
    st0 <- modifyState $ \s -> s{stEstimating = True}
    pushUpdate (estimatePanelView st0)
    result <- liftIO (runReport defaultRuns (stDoc st0))
    st <- modifyState $ \st1 ->
      if stEpoch st1 /= stEpoch st0
        then st1{stEstimating = False} -- raced with an edit: discard; still stale, so the view re-arms
        else
          st1
            { stEstimating = False
            , stStale = False
            , stReport = Just result
            , stPrevReport = case stReport st1 of
                Just (Right r) -> Just r
                _ -> stPrevReport st1
            }
    pure (estimatePanelView st)
  update ToggleAuto = do
    st <- modifyState $ \s -> s{stAuto = not (stAuto s)}
    pure (estimatePanelView st)

------------------------------------------------------------------------------
-- Header rendering
------------------------------------------------------------------------------

headerView :: AppState -> View Header ()
headerView st = el @ att "class" "header" $ do
  el @ att "class" "title" $ text (stTitle st)
  maybe none (\note -> el @ att "class" "note" $ text note) (stNote st)
  maybe none (\msg -> el @ att "class" "status" $ text msg) (stStatus st)
  when (stDirty st) $ el @ att "class" "dirty" $ text "Unsaved changes"
  unless (null (stUndo st)) $
    button Undo (text "Undo") @ att "class" "btn-link"
  button SaveDoc (text "Save") @ att "class" "btn btn-primary"

------------------------------------------------------------------------------
-- TaskTable rendering
------------------------------------------------------------------------------

data FormSlot = SlotEdit Int | SlotQuickTask | SlotQuickResource | SlotQuickAlias
  deriving stock (Eq)

-- | A failed submission: which form, the submitted values, the error.
data FormRetry = FormRetry FormSlot [Text] Text

{- | Per-render (not persisted) table context: a failed form to re-show
with its submitted values, and a quick-add row to refocus after a
successful add (burst entry).
-}
data TableMode = TableMode
  { tmRetry :: Maybe FormRetry
  , tmFocusQuick :: Maybe FormSlot
  }

plain :: TableMode
plain = TableMode{tmRetry = Nothing, tmFocusQuick = Nothing}

tableView :: AppState -> TableMode -> View TaskTable ()
tableView st mode = do
  resourcesSection st mode
  durationsSection st mode
  taskSection st mode

-- | Editing target, with a failed edit submission taking precedence.
effectiveEditing :: AppState -> TableMode -> Maybe (Int, Int)
effectiveEditing st mode = case tmRetry mode of
  Just (FormRetry (SlotEdit i) _ _) -> Just (i, 0)
  _ -> stEditing st

{- | Values + error + focus for a form slot: a failed submission re-shows
what was typed; otherwise the spec's initial values.
-}
formState :: TableMode -> FormSlot -> FormSpec -> Int -> ([Text], Maybe Text, Int)
formState mode slot spec defaultFocus = case tmRetry mode of
  Just (FormRetry s values err)
    | s == slot -> (take n (values <> repeat ""), Just err, 0)
  _
    | tmFocusQuick mode == Just slot -> (initials, Nothing, 0)
    | otherwise -> (initials, Nothing, defaultFocus)
 where
  n = length (formFields spec)
  initials = fieldInitial <$> formFields spec

-- * Vocabulary sections

resourcesSection :: AppState -> TableMode -> View TaskTable ()
resourcesSection st mode = do
  el @ att "class" "vocab-line" $ do
    button ToggleResources (text (chevron (stShowResources st) <> " Resources")) @ att "class" "btn-link vocab-toggle"
    unless (stShowResources st) $ el @ att "class" "vocab-summary" $ text resourceSummary
  when (stShowResources st) $ el @ att "class" "vgrid" $ do
    el @ att "class" "vrow thead" $ do
      el $ text "Resource"
      el $ text "Capacity"
      el $ text "Used by"
      el none
    mapM_ resourceRow (EditorView.resourceRows (stDoc st))
    quickForm "vrow vrow-form quick-row" "qr" QuickAddResource quickSpec quickValues quickErr quickFocus
 where
  quickSpec = withLabel 0 "+ Add resource" (lenientResourceSpec Nothing)
  (quickValues, quickErr, quickFocus) = formState mode SlotQuickResource quickSpec (-1)
  resourceSummary = case EditorView.resourceRows (stDoc st) of
    [] -> "(none)"
    rows -> Text.intercalate " · " [EditorView.resourceRowName r <> " ×" <> showT (EditorView.resourceRowCapacity r) | r <- rows]
  resourceRow r
    | Just (i, focus) <- effectiveEditing st mode
    , i == EditorView.resourceRowIndex r =
        editRowForm st mode "vrow vrow-form" i focus
    | otherwise =
        let i = EditorView.resourceRowIndex r
         in el @ att "class" "vrow" $ do
              el @ onClick (EditRow i 0) . att "class" "tcell" $ text (EditorView.resourceRowName r)
              el @ onClick (EditRow i 1) . att "class" "tcell" @ att "data-label" "Capacity" $ text (showT (EditorView.resourceRowCapacity r))
              el @ att "class" "vcell-used" @ att "data-label" "Used by" $ text (usedByText (EditorView.resourceRowUsedBy r))
              deleteCell i

durationsSection :: AppState -> TableMode -> View TaskTable ()
durationsSection st mode = do
  el @ att "class" "vocab-line" $ do
    button ToggleDurations (text (chevron (stShowDurations st) <> " Durations")) @ att "class" "btn-link vocab-toggle"
    unless (stShowDurations st) $ el @ att "class" "vocab-summary" $ text durationSummary
  when (stShowDurations st) $ el @ att "class" "vgrid" $ do
    el @ att "class" "vrow thead" $ do
      el $ text "Duration"
      el $ text "Definition"
      el $ text "Used by"
      el none
    mapM_ durationRow (EditorView.durationRows (stDoc st))
    quickForm "vrow vrow-form quick-row" "qd" QuickAddAlias quickSpec quickValues quickErr quickFocus
 where
  quickSpec = withLabel 0 "+ Add duration" newAliasSpec
  (quickValues, quickErr, quickFocus) = case stAliasDraft st of
    Just draft | noRetryFor SlotQuickAlias -> ([draft, ""], Nothing, 1)
    _ -> formState mode SlotQuickAlias quickSpec (-1)
  noRetryFor slot = case tmRetry mode of
    Just (FormRetry s _ _) -> s /= slot
    Nothing -> True
  durationSummary = case EditorView.durationRows (stDoc st) of
    [] -> "(none)"
    rows -> Text.intercalate " · " [EditorView.durationRowName r <> " " <> EditorView.durationRowDefinition r | r <- rows]
  durationRow r
    | Just (i, focus) <- effectiveEditing st mode
    , i == EditorView.durationRowIndex r =
        editRowForm st mode "vrow vrow-form" i focus
    | otherwise =
        let i = EditorView.durationRowIndex r
         in el @ att "class" "vrow" $ do
              el @ onClick (EditRow i 0) . att "class" "tcell" $ text (EditorView.durationRowName r)
              el @ onClick (EditRow i 1) . att "class" "tcell" @ att "data-label" "Definition" $ text (EditorView.durationRowDefinition r)
              el @ att "class" "vcell-used" @ att "data-label" "Used by" $ text (usedByText (EditorView.durationRowUsedBy r))
              deleteCell i

chevron :: Bool -> Text
chevron expanded = if expanded then "▾" else "▸"

usedByText :: [Text] -> Text
usedByText [] = "unused"
usedByText ts = Text.intercalate ", " ts

-- * Task table

taskSection :: AppState -> TableMode -> View TaskTable ()
taskSection st mode = do
  el @ att "class" "trow thead" $ do
    el $ text "Task"
    el $ text "Resource"
    el $ text "Duration"
    el $ text "After"
    el none
  case EditorView.taskRows (stDoc st) of
    [] -> el @ att "class" "empty" $ text "No tasks yet — sketch one below (a name is enough to start)."
    rows -> mapM_ taskRow rows
  quickForm "trow trow-form quick-row" "qt" QuickAddTask quickSpec quickValues quickErr quickFocus
 where
  quickSpec = withLabel 0 "+ Add task" (lenientTaskSpec (stDoc st) Nothing)
  (quickValues, quickErr, quickFocus) = formState mode SlotQuickTask quickSpec (-1)
  taskRow r
    | Just (i, focus) <- effectiveEditing st mode
    , i == EditorView.taskRowIndex r =
        editRowForm st mode "trow trow-form" i focus
    | otherwise = taskDisplayRow r

taskDisplayRow :: EditorView.TaskRow -> View TaskTable ()
taskDisplayRow r = el @ att "class" "trow" $ do
  let i = EditorView.taskRowIndex r
  el @ onClick (EditRow i 0) . att "class" "tcell tc-name" $ do
    el @ att "class" ("task-name depth-" <> showT (EditorView.taskRowDepth r)) $
      text (EditorView.taskRowName r)
    unless (Text.null (EditorView.taskRowDescription r)) $
      el @ onClick (EditRow i 4) . att "class" "task-description" $
        text (EditorView.taskRowDescription r)
  el @ onClick (EditRow i 1) . att "class" "tcell" @ att "data-label" "Resource" $
    cellText (EditorView.taskRowResource r)
  el @ onClick (EditRow i 2) . att "class" "tcell" @ att "data-label" "Duration" $
    cellText (EditorView.taskRowDuration r)
  el @ onClick (EditRow i 3) . att "class" "tcell" @ att "data-label" "After" $
    cellText (Text.intercalate ", " (EditorView.taskRowAfter r))
  deleteCell i
  unless (null (EditorView.taskRowIssues r)) $
    el @ att "class" "row-issues" $ do
      text ("! " <> Text.intercalate "  ·  " (EditorView.taskRowIssues r))
      maybe
        none
        (\name -> button (DefineResource name) (text ("Define " <> name <> " ×1")) @ att "class" "btn-link btn-fix")
        (EditorView.taskRowUndefinedResource r)
      maybe
        none
        (\name -> button (DefineAlias name) (text ("Define " <> name <> "…")) @ att "class" "btn-link btn-fix")
        (EditorView.taskRowUndefinedDuration r)

-- | An empty cell still needs a visible click target.
cellText :: Text -> View TaskTable ()
cellText t
  | Text.null t = el @ att "class" "cell-empty" $ text "—"
  | otherwise = text t

deleteCell :: Int -> View TaskTable ()
deleteCell i =
  el @ att "class" "tactions" $
    button (DeleteRow i) (text "×") @ att "class" "btn-link btn-del"

-- * Row forms

-- | The in-place edit form for the doc element at the given index.
editRowForm :: AppState -> TableMode -> Text -> Int -> Int -> View TaskTable ()
editRowForm st mode cls i focus =
  case drop i (stDoc st) of
    (element : _) ->
      let spec = lenientEditSpec (stDoc st) element
          (values, err, focus') = case tmRetry mode of
            Just (FormRetry (SlotEdit j) vals e)
              | j == i -> (take (length (formFields spec)) (vals <> repeat ""), Just e, 0)
            _ -> (fieldInitial <$> formFields spec, Nothing, focus)
       in rowForm cls ("e" <> showT i) (CommitRow i) spec values err focus' "✓"
    [] -> none

-- | A quick-add form row.
quickForm :: Text -> Text -> Action TaskTable -> FormSpec -> [Text] -> Maybe Text -> Int -> View TaskTable ()
quickForm cls dlPrefix action spec values err focus =
  rowForm cls dlPrefix action spec values err focus "+"

rowForm :: Text -> Text -> Action TaskTable -> FormSpec -> [Text] -> Maybe Text -> Int -> Text -> View TaskTable ()
rowForm cls dlPrefix action spec values err focus submitLabel = do
  maybe none (\e -> el @ att "class" "form-error row-error" $ text ("! " <> e)) err
  form action @ att "class" cls $ do
    mapM_ renderInput (zip3 [0 ..] (formFields spec) values)
    submit (text submitLabel) @ att "class" "btn btn-mini"
 where
  renderInput (i, f, val) =
    field (fieldNameFor i) $ do
      input TextInput
        @ value val
        . att "list" dlId
        . att "placeholder" (fieldLabel f)
        . att "class" ("cell-input fi-" <> showT i)
        . att "data-onkeydown-escape" cancelEditAction
        . (if i == focus then att "autofocus" "" else id)
      tag "datalist" @ att "id" dlId $
        mapM_ (\c -> tag "option" @ att "value" c $ none) (fieldCompletions f)
   where
    dlId = "dl-" <> dlPrefix <> "-" <> showT i

{- | Escape cancels the edit: key events dispatch off the focused element
itself, and there is no @HyperView (FormFields id)@ instance to hang
'onKeyDown' on, so the @data-onkeydown-escape@ attribute is written by
hand; the client resolves its target via the enclosing view element.
-}
cancelEditAction :: Text
cancelEditAction = encodeAction CancelEdit

-- | Override a field's label (quick-add rows label their name field).
withLabel :: Int -> Text -> FormSpec -> FormSpec
withLabel i label spec =
  spec
    { formFields =
        [ if j == i then f{fieldLabel = label} else f
        | (j, f) <- zip [0 ..] (formFields spec)
        ]
    }

fieldValues :: Int -> Form -> [Text]
fieldValues n submittedForm =
  [ either (const "") id (parseField @Text (fieldKey i) submittedForm)
  | i <- [0 .. n - 1]
  ]

fieldKey :: Int -> BS8.ByteString
fieldKey i = BS8.pack ("field-" <> show i)

fieldNameFor :: Int -> FieldName Text
fieldNameFor i = fromString ("field-" <> show i)

------------------------------------------------------------------------------
-- Estimate rendering
------------------------------------------------------------------------------

estimatePanelView :: AppState -> View EstimatePanel ()
estimatePanelView st = el @ att "class" "estimate" $ do
  el @ att "class" "estimate-controls" $ do
    el @ att "class" "panel-heading" $ text "Estimate"
    if stEstimating st
      then el @ att "class" "estimate-running" $ text "recalculating…"
      else button Recalc (text "Run estimate") @ att "class" "btn btn-mini"
    button ToggleAuto (text (if stAuto st then "auto: on" else "auto: off")) @ att "class" "btn-link"
  -- Self-arming recalculation: rendered exactly when a run is needed.
  when (stAuto st && stStale st && not (stEstimating st)) $
    el @ onLoad Recalc 250 $
      none
  when (not (stAuto st) && stStale st && not (stEstimating st)) $
    el @ att "class" "estimate-stale" $
      text "Project changed since this estimate — Run estimate to refresh."
  case stReport st of
    Nothing ->
      unless (stEstimating st) $
        el @ att "class" "empty" $
          text "No estimate yet."
    Just (Left err) -> el @ att "class" "form-error" $ text ("! " <> err)
    Just (Right report) -> reportView (stEstimating st) (stPrevReport st) report

reportView :: Bool -> Maybe Report -> Report -> View EstimatePanel ()
reportView dimmed prev report = el @ att "class" (if dimmed then "report dim" else "report") $ do
  el @ att "class" "estimate-summary" $
    text
      ( showT (reportRuns report)
          <> " simulation runs"
          <> if reportTasksExcluded report > 0
            then " (" <> showT (reportTasksIncluded report) <> " tasks; " <> showT (reportTasksExcluded report) <> " excluded)"
            else ""
      )
  el @ att "class" "estimate-summary" $ do
    text ("mean " <> f1 mean)
    meanDelta
  el @ att "class" "estimate-quantiles" $
    mapM_ quantileChip ([5, 25, 50, 75, 90, 95] :: [Word])
  histogram (Stats.histogram 10 (Stats.p99range samples) samples)
 where
  samples = reportSamples report
  mean = Stats.weightedAverage samples
  prevSamples = reportSamples <$> prev
  meanDelta = case prevSamples of
    Just ps
      | delta <- mean - Stats.weightedAverage ps
      , f1 delta /= "0.0" && f1 delta /= "-0.0" ->
          el @ att "class" (deltaClass delta) $ text (" " <> signed delta)
    _ -> none
  quantileChip p = el @ att "class" "quantile" $ do
    let cur = Stats.quantile p 100 samples
    case prevSamples of
      Just ps
        | old <- Stats.quantile p 100 ps
        , f1 old /= f1 cur ->
            text ("p" <> showT p <> " " <> f1 old <> " → " <> f1 cur)
      _ -> text ("p" <> showT p <> " " <> f1 cur)
  deltaClass d = if d > 0 then "delta delta-up" else "delta delta-down"
  signed d = (if d > 0 then "+" else "") <> f1 d

histogram :: [Stats.HistogramEntry] -> View EstimatePanel ()
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

------------------------------------------------------------------------------
-- Styles
------------------------------------------------------------------------------

styles :: View c ()
styles =
  style
    "\
    \.app { font-family: system-ui, -apple-system, \"Segoe UI\", sans-serif; color: #0b0b0b; max-width: 1280px; margin: 0 auto; padding: 16px; }\
    \.header { display: flex; flex-wrap: wrap; align-items: center; gap: 12px; margin-bottom: 16px; }\
    \.title { font-weight: 600; }\
    \.note, .status { color: #52514e; }\
    \.dirty { color: #d03b3b; }\
    \.columns { display: flex; gap: 24px; align-items: flex-start; }\
    \.column-main { flex: 3; min-width: 0; }\
    \.column-est { flex: 2; min-width: 0; }\
    \.panel-heading { font-weight: 600; }\
    \.vocab-line { margin: 2px 0; color: #52514e; display: flex; align-items: baseline; gap: 8px; }\
    \.vocab-toggle { font-weight: 600; color: #0b0b0b; padding-left: 0; }\
    \.vocab-summary { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }\
    \.vgrid { margin: 4px 0 14px; }\
    \.trow { display: grid; grid-template-columns: minmax(9rem, 2.2fr) 1fr 1.1fr 1.4fr 2rem; gap: 0 10px; align-items: start; border-bottom: 1px solid #e1e0d9; padding: 3px 0; }\
    \.vrow { display: grid; grid-template-columns: 1.4fr 0.9fr 2.4fr 2rem; gap: 0 10px; align-items: start; border-bottom: 1px solid #e1e0d9; padding: 3px 0; }\
    \.thead { border-bottom: 1px solid #c3c2b7; color: #898781; font-weight: 600; }\
    \.tcell { cursor: pointer; padding: 3px 4px; border-radius: 3px; min-height: 1.3em; }\
    \.tcell:hover { background: #f4f3ee; }\
    \.cell-empty { color: #c3c2b7; }\
    \.vcell-used { color: #52514e; padding: 3px 4px; }\
    \.task-name { font-weight: 500; }\
    \.task-name.depth-1 { padding-left: 16px; }\
    \.task-name.depth-2 { padding-left: 32px; }\
    \.task-name.depth-3 { padding-left: 48px; }\
    \.task-name.depth-4 { padding-left: 64px; }\
    \.task-description { color: #52514e; font-size: 0.9em; }\
    \.tactions { text-align: right; }\
    \.btn-del { color: #898781; padding: 0 6px; }\
    \.btn-del:hover { color: #d03b3b; }\
    \.row-issues { grid-column: 1 / -1; color: #d03b3b; font-size: 0.9em; padding: 0 4px 3px; }\
    \.btn-fix { padding: 0 8px; }\
    \.cell-input { width: 100%; box-sizing: border-box; padding: 3px 6px; border: 1px solid #c3c2b7; border-radius: 4px; font: inherit; }\
    \.quick-row .cell-input { border-style: dashed; }\
    \.trow-form button[type=submit], .vrow-form button[type=submit] { grid-column: -2; grid-row: 1; }\
    \.trow-form .fi-4 { grid-column: 1 / -2; grid-row: 2; margin-top: 4px; }\
    \.row-error { padding: 2px 4px; }\
    \.empty { color: #898781; padding: 8px 0; }\
    \.btn { background: #2a78d6; color: #fff; border: none; border-radius: 4px; padding: 6px 12px; cursor: pointer; }\
    \.btn-mini { padding: 3px 10px; }\
    \.btn-primary { background: #2a78d6; }\
    \.btn-link { background: none; color: #2a78d6; border: none; cursor: pointer; padding: 2px 6px; }\
    \.form-error { color: #d03b3b; margin: 4px 0; }\
    \.estimate { border-left: 1px solid #e1e0d9; padding-left: 24px; }\
    \.estimate-controls { display: flex; align-items: center; gap: 10px; margin-bottom: 8px; }\
    \.estimate-running { color: #52514e; }\
    \.estimate-stale { color: #d03b3b; margin-bottom: 8px; }\
    \.estimate-summary { margin-bottom: 4px; }\
    \.estimate-quantiles { display: flex; flex-wrap: wrap; gap: 12px; margin: 8px 0; color: #52514e; }\
    \.report.dim { opacity: 0.55; }\
    \.delta { font-variant-numeric: tabular-nums; }\
    \.delta-up { color: #d03b3b; }\
    \.delta-down { color: #2e7d32; }\
    \.hist { display: flex; flex-direction: column; gap: 2px; margin-top: 8px; }\
    \.hist-row { display: grid; grid-template-columns: 64px 1fr 48px; align-items: center; gap: 8px; }\
    \.hist-bound { color: #52514e; text-align: right; font-variant-numeric: tabular-nums; }\
    \.hist-bar-track { background: #cde2fb; border-radius: 4px; height: 18px; overflow: hidden; }\
    \.hist-bar-fill { background: #2a78d6; height: 100%; border-radius: 0 4px 4px 0; }\
    \.hist-pct { color: #52514e; font-variant-numeric: tabular-nums; }\
    \@media (max-width: 800px) {\
    \  .app { padding: 12px; }\
    \  .columns { flex-direction: column; gap: 16px; }\
    \  .column-main, .column-est { width: 100%; flex: none; }\
    \  .thead { display: none; }\
    \  .trow, .vrow { display: block; border: 1px solid #e1e0d9; border-radius: 6px; padding: 6px 10px; margin: 8px 0; }\
    \  .tcell[data-label]::before, .vcell-used[data-label]::before { content: attr(data-label) \": \"; color: #898781; }\
    \  .task-name { font-weight: 600; }\
    \  .cell-input { margin: 3px 0; }\
    \  .btn { padding: 10px 16px; }\
    \  .btn-mini { padding: 6px 12px; }\
    \  .btn-link { padding: 8px 10px; }\
    \  .estimate { border-left: none; padding-left: 0; border-top: 1px solid #e1e0d9; padding-top: 16px; }\
    \}\
    \@media (prefers-color-scheme: dark) {\
    \  .app { background: #1a1a19; color: #ffffff; }\
    \  .note, .status, .vocab-line, .vocab-summary, .task-description, .vcell-used, .estimate-quantiles, .estimate-running, .hist-bound, .hist-pct { color: #c3c2b7; }\
    \  .vocab-toggle { color: #ffffff; }\
    \  .thead { color: #898781; border-bottom-color: #383835; }\
    \  .trow, .vrow { border-color: #2c2c2a; }\
    \  .tcell:hover { background: #242422; }\
    \  .cell-empty { color: #52514e; }\
    \  .empty, .btn-del { color: #898781; }\
    \  .btn, .btn-primary, .hist-bar-fill { background: #3987e5; }\
    \  .btn-link { color: #3987e5; }\
    \  .cell-input { background: #1a1a19; color: #fff; border-color: #383835; }\
    \  .delta-down { color: #66bb6a; }\
    \  .estimate { border-left-color: #2c2c2a; border-top-color: #2c2c2a; }\
    \  .hist-bar-track { background: #184f95; }\
    \}"
