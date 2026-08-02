{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The editor page for one document, three HyperViews (see web/DESIGN.md):

* 'Header' — title, project picker, dirty flag, status toast, Undo, Save.
* 'TaskTable' — the vocabulary sections and the task table. Rows edit in
  place (click a cell → the row becomes a form with that field focused);
  the last row of each section is a permanent quick-add form. Every
  committed change pushes fresh renders of the other views.
* 'EstimatePanel' — the live estimate. Whenever the state is stale and
  auto-estimate is on, the panel renders an @onLoad Recalc@ element, so
  the client immediately asks for a recalculation; 'Recalc' blocks on
  the simulation (each action runs in its own thread, and a newer action
  on the same view cancels the running one, so a burst of edits simply
  restarts the run). An epoch counter discards results that raced with
  a newer edit — the stale state then re-arms the @onLoad@, making the
  loop self-correcting.

Which document these act on comes from the URL, not from the view: see
"Web.State".'requireDocument'. The page also renders "Web.Files".'FileBar'
so the open-document strip is present while editing.
-}
module Web.Editor (
  Header (..),
  TaskTable (..),
  EstimatePanel (..),
  editorPage,
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
import Editor.View qualified as EditorView
import Effectful (IOE, liftIO)
import Effectful.Reader.Dynamic (Reader)
import Numeric (showFFloat)
import UncertainGantt qualified as UG
import UncertainGantt.Lang.Types (
  ResourceDescription (ResourceDescription),
  TaskDescription (TaskDescription),
  unDurationAlias,
  unResource,
 )
import UncertainGantt.Sim.Stats qualified as Stats
import UncertainGantt.ToText (toText)
import Web.Capability (DocHandle (dhApplyChange, dhKey, dhModify, dhSave))
import Web.Docs (DocState (..), ServerState)
import Web.Files (Action (FileBarRefresh), FileBar (FileBar), fileBarView)
import Web.Hyperbole
import Web.Hyperbole.Effect.Request (formBody)
import Web.Route (AppRoute (RouteEdit), DocKey (DocKey, dkFile, dkProject), docKeyTitle)
import Web.State (
  Adapters,
  requireDocHandle,
 )
import Web.Styles (styles)

------------------------------------------------------------------------------
-- Page
------------------------------------------------------------------------------

editorPage ::
  (Hyperbole :> es) =>
  ServerState ->
  DocKey ->
  DocState ->
  Page es '[FileBar, Header, TaskTable, EstimatePanel]
editorPage server key doc = do
  -- Distinguishes tabs when several documents are open at once.
  pageTitle (docKeyTitle key)
  pure $ do
    styles
    el @ att "class" "app" $ do
      hyper FileBar (fileBarView (Just key) server)
      hyper Header (headerView key doc)
      el @ att "class" "columns" $ do
        el @ att "class" "column column-main" $ hyper TaskTable (tableView doc plain)
        el @ att "class" "column column-est" $ hyper EstimatePanel (estimatePanelView doc)

------------------------------------------------------------------------------
-- Header
------------------------------------------------------------------------------

data Header = Header
  deriving stock (Generic)
  deriving anyclass (ViewId)

instance (Reader Adapters :> es, IOE :> es) => HyperView Header es where
  data Action Header = SaveDoc | Undo | HeaderRefresh
    deriving stock (Generic)
    deriving anyclass (ViewAction)

  type Require Header = '[TaskTable, EstimatePanel, FileBar]

  update HeaderRefresh = do
    (doc, handle) <- requireDocHandle
    pure (headerView handle.dhKey doc)
  update SaveDoc = do
    (_, handle) <- requireDocHandle
    doc' <- handle.dhSave
    trigger FileBar FileBarRefresh
    pure (headerView handle.dhKey doc')
  update Undo = do
    (_, handle) <- requireDocHandle
    doc <- handle.dhModify $ \d -> case dsUndo d of
      [] -> d
      (previous : rest) ->
        d
          { dsDoc = previous
          , dsUndo = rest
          , dsEpoch = dsEpoch d + 1
          , dsEditing = Nothing
          , dsAliasDraft = Nothing
          , dsStale = True
          , dsDirty = True
          , dsStatus = Just "Undone"
          , dsCloseArmed = False
          }
    trigger TaskTable TableRefresh
    trigger EstimatePanel Refresh
    trigger FileBar FileBarRefresh
    pure (headerView handle.dhKey doc)

headerView :: DocKey -> DocState -> View Header ()
headerView key doc = el @ att "class" "header" $ do
  el @ att "class" "title" $ text (docKeyTitle key)
  if hasPicker then projectPicker key doc else none
  -- The only note a multi-project file produces says which project is being
  -- edited, which is exactly what the picker already shows.
  unless hasPicker $
    maybe none (\note -> el @ att "class" "note" $ text note) (dsNote doc)
  maybe none (\msg -> el @ att "class" "status" $ text msg) (dsStatus doc)
  when (dsDirty doc) $ el @ att "class" "dirty" $ text "Unsaved changes"
  unless (null (dsUndo doc)) $
    button Undo (text "Undo") @ att "class" "btn-link"
  button SaveDoc (text "Save") @ att "class" "btn btn-primary"
 where
  hasPicker = length (dsProjects doc) > 1

{- | A multi-project TOML file gets links to its other projects. They are
plain links, not actions: each project is its own document at its own URL.
-}
projectPicker :: DocKey -> DocState -> View Header ()
projectPicker key doc = el @ att "class" "projects" $ do
  el @ att "class" "projects-label" $ text "Projects:"
  mapM_ projectLink (dsProjects doc)
 where
  projectLink project =
    route (RouteEdit (DocKey{dkFile = dkFile key, dkProject = Just project})) (text project)
      @ att "class" (if dkProject key == Just project then "project-link active" else "project-link")

------------------------------------------------------------------------------
-- TaskTable
------------------------------------------------------------------------------

data TaskTable = TaskTable
  deriving stock (Generic)
  deriving anyclass (ViewId)

instance (Reader Adapters :> es, IOE :> es) => HyperView TaskTable es where
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

  type Require TaskTable = '[Header, EstimatePanel, FileBar]

  update TableRefresh = do
    (doc, _) <- requireDocHandle
    pure (tableView doc plain)
  update (EditRow i focus) = do
    (_, handle) <- requireDocHandle
    doc <- handle.dhModify $ \d -> d{dsEditing = Just (i, focus)}
    pure (tableView doc plain)
  update CancelEdit = do
    (_, handle) <- requireDocHandle
    doc <- handle.dhModify $ \d -> d{dsEditing = Nothing, dsAliasDraft = Nothing}
    pure (tableView doc plain)
  update (CommitRow i) = do
    submitted <- formBody
    (doc0, handle) <- requireDocHandle
    case drop i (dsDoc doc0) of
      (element : _) -> do
        let spec = lenientEditSpec (dsDoc doc0) element
            values = fieldValues (length (formFields spec)) submitted
        case formParse spec values of
          Left err -> pure (tableView doc0 plain{tmRetry = Just (FormRetry (SlotEdit i) values err)})
          Right element' -> do
            doc <- handle.dhApplyChange Nothing (applyOp (OpReplace i element')) id
            refreshOthers
            pure (tableView doc plain)
      [] -> pure (tableView doc0 plain)
  update (DeleteRow i) = do
    (doc0, handle) <- requireDocHandle
    case drop i (dsDoc doc0) of
      (element : _) -> do
        let status = "Deleted \"" <> elementName element <> "\" — Undo to restore"
        doc <- handle.dhApplyChange (Just status) (applyOp (OpDelete i)) id
        refreshOthers
        pure (tableView doc plain)
      [] -> pure (tableView doc0 plain)
  update QuickAddTask = do
    (doc0, handle) <- requireDocHandle
    quickAdd handle doc0 SlotQuickTask (\d -> lenientTaskSpec d Nothing)
  update QuickAddResource = do
    (doc0, handle) <- requireDocHandle
    quickAdd handle doc0 SlotQuickResource (const (lenientResourceSpec Nothing))
  update QuickAddAlias = do
    (doc0, handle) <- requireDocHandle
    quickAdd handle doc0 SlotQuickAlias (const newAliasSpec)
  update ToggleResources = do
    (_, handle) <- requireDocHandle
    doc <- handle.dhModify $ \d -> d{dsShowResources = not (dsShowResources d)}
    pure (tableView doc plain)
  update ToggleDurations = do
    (_, handle) <- requireDocHandle
    doc <- handle.dhModify $ \d -> d{dsShowDurations = not (dsShowDurations d)}
    pure (tableView doc plain)
  update (DefineResource resource) = do
    (_, handle) <- requireDocHandle
    let element = ElemResource (ResourceDescription (fromString (Text.unpack resource)) 1)
    doc <-
      handle.dhApplyChange
        (Just ("Defined resource " <> resource <> " ×1"))
        (applyOp (OpInsert element))
        (\d -> d{dsShowResources = True, dsEditing = Just (length (dsDoc d) - 1, 1)})
    refreshOthers
    pure (tableView doc plain)
  update (DefineAlias alias) = do
    (_, handle) <- requireDocHandle
    doc <- handle.dhModify $ \d -> d{dsShowDurations = True, dsAliasDraft = Just alias, dsEditing = Nothing}
    pure (tableView doc plain)

{- | Handle a quick-add form submission for one of the three sections.
Takes the handle and its snapshot from the caller rather than deriving
them itself — the caller (one @update@ case) is already the request
boundary, so this stays a function of what it's handed, not of the URL.
The handle's row is spelled out as @Reader TaskTable : es@ (matching
'refreshOthers' below, per 'Web.Hyperbole.trigger's own requirement)
rather than a bare @es@, so it's forced to be the very row @handle@ was
minted in at the call site, not a fresh one 'quickAdd' wraps on top.
-}
quickAdd ::
  (Hyperbole :> es, Reader Adapters :> es, IOE :> es) =>
  DocHandle (Reader TaskTable : es) ->
  DocState ->
  FormSlot ->
  (Doc -> FormSpec) ->
  Eff (Reader TaskTable : es) (View TaskTable ())
quickAdd handle doc0 slot mkSpec = do
  submitted <- formBody
  let spec = mkSpec (dsDoc doc0)
      values = fieldValues (length (formFields spec)) submitted
  case formParse spec values of
    Left err -> pure (tableView doc0 plain{tmRetry = Just (FormRetry slot values err)})
    Right element -> do
      doc <- handle.dhApplyChange Nothing (applyOp (OpInsert element)) id
      refreshOthers
      pure (tableView doc plain{tmFocusQuick = Just slot})

{- | After a doc change from the table: have the client re-request the
other views. This must be 'trigger', not 'pushUpdateTo': form
submissions arrive over HTTP, where pushes are silently dropped but
triggers ride back as response metadata.
-}
refreshOthers :: (Hyperbole :> es, Reader Adapters :> es, IOE :> es) => Eff (Reader TaskTable : es) ()
refreshOthers = do
  trigger Header HeaderRefresh
  trigger EstimatePanel Refresh
  trigger FileBar FileBarRefresh

elementName :: Element -> Text
elementName = \case
  ElemResource (ResourceDescription r _) -> toText (unResource r)
  ElemAlias a _ -> toText (unDurationAlias a)
  ElemTask (TaskDescription n _ _ _ _) -> toText (UG.unTaskName n)

------------------------------------------------------------------------------
-- EstimatePanel
------------------------------------------------------------------------------

data EstimatePanel = EstimatePanel
  deriving stock (Generic)
  deriving anyclass (ViewId)

instance (Reader Adapters :> es, IOE :> es) => HyperView EstimatePanel es where
  data Action EstimatePanel = Refresh | Recalc | ToggleAuto
    deriving stock (Generic)
    deriving anyclass (ViewAction)

  -- A newer action cancels the running one server-side; Replace makes the
  -- client keep sending them instead of dropping while one is in flight.
  -- The cancellation map is per client, so tabs on other documents are
  -- unaffected.
  type Concurrency EstimatePanel = 'Replace

  -- Triggered after every doc change. Arriving on this view, it cancels
  -- any in-flight Recalc — whose snapshot predates the change, so its
  -- result was stale anyway — which is why it must clear dsEstimating;
  -- the stale+auto render then re-arms the onLoad loop.
  update Refresh = do
    (_, handle) <- requireDocHandle
    doc <- handle.dhModify $ \d -> d{dsEstimating = False}
    pure (estimatePanelView doc)
  update Recalc = do
    (_, handle) <- requireDocHandle
    doc0 <- handle.dhModify $ \d -> d{dsEstimating = True}
    pushUpdate (estimatePanelView doc0)
    result <- liftIO (runReport defaultRuns (dsDoc doc0))
    doc <- handle.dhModify $ \d ->
      if dsEpoch d /= dsEpoch doc0
        then d{dsEstimating = False} -- raced with an edit: discard; still stale, so the view re-arms
        else
          d
            { dsEstimating = False
            , dsStale = False
            , dsReport = Just result
            , dsPrevReport = case dsReport d of
                Just (Right r) -> Just r
                _ -> dsPrevReport d
            }
    pure (estimatePanelView doc)
  update ToggleAuto = do
    (_, handle) <- requireDocHandle
    doc <- handle.dhModify $ \d -> d{dsAuto = not (dsAuto d)}
    pure (estimatePanelView doc)

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

tableView :: DocState -> TableMode -> View TaskTable ()
tableView doc mode = do
  resourcesSection doc mode
  durationsSection doc mode
  taskSection doc mode

-- | Editing target, with a failed edit submission taking precedence.
effectiveEditing :: DocState -> TableMode -> Maybe (Int, Int)
effectiveEditing doc mode = case tmRetry mode of
  Just (FormRetry (SlotEdit i) _ _) -> Just (i, 0)
  _ -> dsEditing doc

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

resourcesSection :: DocState -> TableMode -> View TaskTable ()
resourcesSection doc mode = do
  el @ att "class" "vocab-line" $ do
    button ToggleResources (text (chevron (dsShowResources doc) <> " Resources")) @ att "class" "btn-link vocab-toggle"
    unless (dsShowResources doc) $ el @ att "class" "vocab-summary" $ text resourceSummary
  when (dsShowResources doc) $ el @ att "class" "vgrid" $ do
    el @ att "class" "vrow thead" $ do
      el $ text "Resource"
      el $ text "Capacity"
      el $ text "Used by"
      el none
    mapM_ resourceRow (EditorView.resourceRows (dsDoc doc))
    quickForm "vrow vrow-form quick-row" "qr" QuickAddResource quickSpec quickValues quickErr quickFocus
 where
  quickSpec = withLabel 0 "+ Add resource" (lenientResourceSpec Nothing)
  (quickValues, quickErr, quickFocus) = formState mode SlotQuickResource quickSpec (-1)
  resourceSummary = case EditorView.resourceRows (dsDoc doc) of
    [] -> "(none)"
    rows -> Text.intercalate " · " [EditorView.resourceRowName r <> " ×" <> showT (EditorView.resourceRowCapacity r) | r <- rows]
  resourceRow r
    | Just (i, focus) <- effectiveEditing doc mode
    , i == EditorView.resourceRowIndex r =
        editRowForm doc mode "vrow vrow-form" i focus
    | otherwise =
        let i = EditorView.resourceRowIndex r
         in el @ att "class" "vrow" $ do
              el @ onClick (EditRow i 0) . att "class" "tcell" $ text (EditorView.resourceRowName r)
              el @ onClick (EditRow i 1) . att "class" "tcell" @ att "data-label" "Capacity" $ text (showT (EditorView.resourceRowCapacity r))
              el @ att "class" "vcell-used" @ att "data-label" "Used by" $ text (usedByText (EditorView.resourceRowUsedBy r))
              deleteCell i

durationsSection :: DocState -> TableMode -> View TaskTable ()
durationsSection doc mode = do
  el @ att "class" "vocab-line" $ do
    button ToggleDurations (text (chevron (dsShowDurations doc) <> " Durations")) @ att "class" "btn-link vocab-toggle"
    unless (dsShowDurations doc) $ el @ att "class" "vocab-summary" $ text durationSummary
  when (dsShowDurations doc) $ el @ att "class" "vgrid" $ do
    el @ att "class" "vrow thead" $ do
      el $ text "Duration"
      el $ text "Definition"
      el $ text "Used by"
      el none
    mapM_ durationRow (EditorView.durationRows (dsDoc doc))
    quickForm "vrow vrow-form quick-row" "qd" QuickAddAlias quickSpec quickValues quickErr quickFocus
 where
  quickSpec = withLabel 0 "+ Add duration" newAliasSpec
  (quickValues, quickErr, quickFocus) = case dsAliasDraft doc of
    Just draft | noRetryFor SlotQuickAlias -> ([draft, ""], Nothing, 1)
    _ -> formState mode SlotQuickAlias quickSpec (-1)
  noRetryFor slot = case tmRetry mode of
    Just (FormRetry s _ _) -> s /= slot
    Nothing -> True
  durationSummary = case EditorView.durationRows (dsDoc doc) of
    [] -> "(none)"
    rows -> Text.intercalate " · " [EditorView.durationRowName r <> " " <> EditorView.durationRowDefinition r | r <- rows]
  durationRow r
    | Just (i, focus) <- effectiveEditing doc mode
    , i == EditorView.durationRowIndex r =
        editRowForm doc mode "vrow vrow-form" i focus
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

taskSection :: DocState -> TableMode -> View TaskTable ()
taskSection doc mode = do
  el @ att "class" "trow thead" $ do
    el $ text "Task"
    el $ text "Resource"
    el $ text "Duration"
    el $ text "After"
    el none
  case EditorView.taskRows (dsDoc doc) of
    [] -> el @ att "class" "empty" $ text "No tasks yet — sketch one below (a name is enough to start)."
    rows -> mapM_ taskRow rows
  quickForm "trow trow-form quick-row" "qt" QuickAddTask quickSpec quickValues quickErr quickFocus
 where
  quickSpec = withLabel 0 "+ Add task" (lenientTaskSpec (dsDoc doc) Nothing)
  (quickValues, quickErr, quickFocus) = formState mode SlotQuickTask quickSpec (-1)
  taskRow r
    | Just (i, focus) <- effectiveEditing doc mode
    , i == EditorView.taskRowIndex r =
        editRowForm doc mode "trow trow-form" i focus
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
        (\resource -> button (DefineResource resource) (text ("Define " <> resource <> " ×1")) @ att "class" "btn-link btn-fix")
        (EditorView.taskRowUndefinedResource r)
      maybe
        none
        (\alias -> button (DefineAlias alias) (text ("Define " <> alias <> "…")) @ att "class" "btn-link btn-fix")
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
editRowForm :: DocState -> TableMode -> Text -> Int -> Int -> View TaskTable ()
editRowForm doc mode cls i focus =
  case drop i (dsDoc doc) of
    (element : _) ->
      let spec = lenientEditSpec (dsDoc doc) element
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
withLabel i newLabel spec =
  spec
    { formFields =
        [ if j == i then f{fieldLabel = newLabel} else f
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

estimatePanelView :: DocState -> View EstimatePanel ()
estimatePanelView doc = el @ att "class" "estimate" $ do
  el @ att "class" "estimate-controls" $ do
    el @ att "class" "panel-heading" $ text "Estimate"
    if dsEstimating doc
      then el @ att "class" "estimate-running" $ text "recalculating…"
      else button Recalc (text "Run estimate") @ att "class" "btn btn-mini"
    button ToggleAuto (text (if dsAuto doc then "auto: on" else "auto: off")) @ att "class" "btn-link"
  -- Self-arming recalculation: rendered exactly when a run is needed.
  when (dsAuto doc && dsStale doc && not (dsEstimating doc)) $
    el @ onLoad Recalc 250 $
      none
  when (not (dsAuto doc) && dsStale doc && not (dsEstimating doc)) $
    el @ att "class" "estimate-stale" $
      text "Project changed since this estimate — Run estimate to refresh."
  case dsReport doc of
    Nothing ->
      unless (dsEstimating doc) $
        el @ att "class" "empty" $
          text "No estimate yet."
    Just (Left err) -> el @ att "class" "form-error" $ text ("! " <> err)
    Just (Right report) -> reportView (dsEstimating doc) (dsPrevReport doc) report

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
