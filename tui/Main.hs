{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Interactive project editor: build a project out of resources, duration
aliases and tasks, refresh a Monte Carlo completion-time estimate, and
save/load the project as a script file compatible with the CLI.
-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Text.Zipper (TextAlignment (TextAlignment_Left))
import Graphics.Vty qualified as V
import Reflex
import Reflex.Network (networkView)
import Reflex.Vty
import Reflex.Workflow (Workflow (Workflow), workflow)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeBaseName)

import Tui.Doc (
  Doc,
  DocOp (OpDelete, OpInsert, OpReplace),
  Element,
  FormSpec (formFields, formParse, formTitle),
  applyOp,
  docProjectIssues,
  editSpec,
  fromProjectEntry,
  fromStatements,
  newAliasSpec,
  newResourceSpec,
  newTaskSpec,
  renderIssue,
  toProjectEntry,
  toStatements,
 )
import Tui.Estimate (Report, defaultRuns, renderReport, runReport)
import Tui.View qualified as View
import Tui.Widgets (FormResult (formCancel, formSubmit, formValues), Vty, form, keyEv)
import UncertainGantt.Script.Parser (parseScript)
import UncertainGantt.Script.Render (renderDeclarations)
import UncertainGantt.ToText (showText)
import UncertainGantt.Toml (
  ProjectEntry (entryName),
  ProjectsFile (ProjectsFile),
  decodeProjectsFile,
  emptyProjectEntry,
  encodeProjectsFile,
 )

main :: IO ()
main =
  getArgs >>= \case
    [] -> start "project.toml" Nothing
    [path] -> start path Nothing
    [path, project] -> start path (Just (Text.pack project))
    _ -> die "usage: uncertain-gantt-tui [FILE [PROJECT]]"

-- | Everything the app needs to know about where the project came from.
data AppConfig = AppConfig
  { appTitle :: Text
  , appInitialDoc :: Doc
  , appInitialNote :: Maybe Text
  , appSave :: Doc -> IO Text
  }

start :: FilePath -> Maybe Text -> IO ()
start path mbProject
  | ".ug" `isSuffixOf` path = startScript path mbProject
  | otherwise = startToml path mbProject

-- | Legacy @.ug@ script persistence: one project per file.
startScript :: FilePath -> Maybe Text -> IO ()
startScript path mbProject = do
  case mbProject of
    Just _ -> die (path <> " is a .ug script; it holds a single project, so a project name cannot be given")
    Nothing -> pure ()
  exists <- doesFileExist path
  (doc0, dropped) <-
    if not exists
      then pure ([], 0)
      else do
        contents <- readFile path
        case parseScript contents of
          Left (err, _) -> die ("Failed to parse " <> path <> ":\n" <> err)
          Right statements -> pure (fromStatements statements)
  runApp
    AppConfig
      { appTitle = Text.pack path
      , appInitialDoc = doc0
      , appInitialNote =
          if dropped > 0
            then Just ("[" <> showText dropped <> " print/run statements ignored]")
            else Nothing
      , appSave = \doc -> do
          Text.IO.writeFile path (renderDeclarations (toStatements doc))
          pure ("Saved " <> Text.pack path)
      }

{- | TOML persistence (see TOML-FORMAT.md): a file holds many projects;
we edit one and preserve the rest on save.
-}
startToml :: FilePath -> Maybe Text -> IO ()
startToml path mbProject = do
  exists <- doesFileExist path
  entries <-
    if not exists
      then pure []
      else do
        contents <- Text.IO.readFile path
        case decodeProjectsFile contents of
          Left err -> die ("Failed to parse " <> path <> ":\n" <> Text.unpack err)
          Right (ProjectsFile entries) -> pure entries
  (index, entry) <- case mbProject of
    Nothing -> pure $ case entries of
      [] -> (0, emptyProjectEntry (Text.pack (takeBaseName path)))
      (first : _) -> (0, first)
    Just projectName ->
      case List.find ((== projectName) . entryName . snd) (zip [0 ..] entries) of
        Just found -> pure found
        Nothing
          | null entries -> pure (0, emptyProjectEntry projectName)
          | otherwise ->
              die . Text.unpack $
                "No project named \""
                  <> projectName
                  <> "\" in "
                  <> Text.pack path
                  <> ". Available: "
                  <> Text.intercalate ", " (entryName <$> entries)
  runApp
    AppConfig
      { appTitle = Text.pack path <> " · " <> entryName entry
      , appInitialDoc = fromProjectEntry entry
      , appInitialNote =
          if length entries > 1
            then Just ("[file has " <> showText (length entries) <> " projects — editing \"" <> entryName entry <> "\"]")
            else Nothing
      , appSave = \doc -> do
          let entries' = setOrAppend index (toProjectEntry entry doc) entries
          Text.IO.writeFile path (encodeProjectsFile (ProjectsFile entries'))
          pure ("Saved " <> Text.pack path <> " · " <> entryName entry)
      }
 where
  setOrAppend i x xs
    | i < length xs = take i xs <> [x] <> drop (i + 1) xs
    | otherwise = xs <> [x]

data ViewMode = ModeSplit | ModeTabs
  deriving stock (Eq)

data Tab = TabEditor | TabEstimate
  deriving stock (Eq)

-- | Everything the panes can ask the app to do.
data EditorMsg
  = EMsgOp DocOp
  | EMsgMove Int
  | EMsgToggleView
  | EMsgNextTab
  | EMsgQuit
  | EMsgStatus Text

runApp :: AppConfig -> IO ()
runApp cfg = mainWidget def $ initManager_ $ do
  rec docDyn <- foldDyn applyOp (appInitialDoc cfg) docOpEv
      let taskCount = length . View.taskRows <$> docDyn
      selDyn <-
        foldDyn ($) 0 $
          mergeWith
            (.)
            [ ffor (attach (current taskCount) moveEv) $ \(n, delta) sel -> clampSel n (sel + delta)
            , clampSel <$> updated taskCount
            ]

      viewDyn <- foldDyn (\() m -> if m == ModeSplit then ModeTabs else ModeSplit) ModeSplit toggleViewEv
      tabDyn <- foldDyn (\() t -> if t == TabEditor then TabEstimate else TabEditor) TabEditor nextTabEv

      refreshF5 <- keyEv (V.KFun 5) []
      refreshCtrl <- keyEv (V.KChar 'r') [V.MCtrl]
      reportEv <-
        performEvent $
          liftIO . runReport defaultRuns
            <$> tag (current docDyn) (leftmost [refreshF5, refreshCtrl])
      reportDyn <- holdDyn Nothing (Just <$> reportEv)
      staleDyn <- holdDyn False $ leftmost [False <$ reportEv, True <$ docOpEv]
      let estimateTextDyn = estimateText <$> (fmap renderIssue . snd . docProjectIssues <$> docDyn) <*> staleDyn <*> reportDyn

      saveKeyEv <- keyEv (V.KChar 's') [V.MCtrl]
      savedEv <-
        performEvent $
          liftIO . appSave cfg <$> tag (current docDyn) saveKeyEv

      -- Quitting with unsaved changes requires a second quit to confirm.
      dirtyDyn <- holdDyn False $ leftmost [True <$ docOpEv, False <$ savedEv]
      armedDyn <- holdDyn False $ leftmost [True <$ quitBlockedEv, False <$ docOpEv, False <$ savedEv]
      ctrlQ <- keyEv (V.KChar 'q') [V.MCtrl]
      let quitReqEv = leftmost [ctrlQ, paneQuitEv]
          quitDecisionEv = attach ((,) <$> current dirtyDyn <*> current armedDyn) quitReqEv
          quitOkEv = () <$ ffilter (\((dirty, armed), _) -> not dirty || armed) quitDecisionEv
          quitBlockedEv = () <$ ffilter (\((dirty, armed), _) -> dirty && not armed) quitDecisionEv

      statusDyn <-
        holdDyn initialStatus $
          leftmost
            [ "Unsaved changes! C-s to save, or quit again to discard" <$ quitBlockedEv
            , statusMsgEv
            , savedEv
            , hints <$ docOpEv
            ]

      paneEvs <- networkView $
        ffor ((,) <$> viewDyn <*> tabDyn) $ \paneCfg ->
          renderPanes paneCfg (appTitle cfg) docDyn selDyn estimateTextDyn statusDyn
      paneMsgEv <- switchHold never paneEvs
      let docOpEv = fforMaybe paneMsgEv $ \case EMsgOp op -> Just op; _ -> Nothing
          moveEv = fforMaybe paneMsgEv $ \case EMsgMove d -> Just d; _ -> Nothing
          toggleViewEv = fforMaybe paneMsgEv $ \case EMsgToggleView -> Just (); _ -> Nothing
          nextTabEv = fforMaybe paneMsgEv $ \case EMsgNextTab -> Just (); _ -> Nothing
          paneQuitEv = fforMaybe paneMsgEv $ \case EMsgQuit -> Just (); _ -> Nothing
          statusMsgEv = fforMaybe paneMsgEv $ \case EMsgStatus s -> Just s; _ -> Nothing
  ctrlC <- keyEv (V.KChar 'c') [V.MCtrl]
  pure $ leftmost [ctrlC, quitOkEv]
 where
  initialStatus = case appInitialNote cfg of
    Just note -> note <> " " <> hints
    Nothing -> hints

clampSel :: Int -> Int -> Int
clampSel n sel = max 0 (min (n - 1) sel)

hints :: Text
hints = "j/k move | Enter edit | a add | x del | R resources | D durations | C-r estimate | C-s save | v view | C-q quit"

renderPanes ::
  (Vty t m, Adjustable t m) =>
  (ViewMode, Tab) ->
  Text ->
  Dynamic t Doc ->
  Dynamic t Int ->
  Dynamic t Text ->
  Dynamic t Text ->
  m (Event t EditorMsg)
renderPanes (view, tab) title docDyn selDyn estimateTextDyn statusDyn = col $ do
  grout (fixed 1) $ text (pure headerLine)
  msg <- grout flex $ case (view, tab) of
    (ModeSplit, _) -> row $ do
      msg <- grout flex $ framed " Editor " editor
      grout flex $ framed " Estimate " estimateView
      pure msg
    (ModeTabs, TabEditor) ->
      framed " Editor | Tab -> Estimate " editor
    (ModeTabs, TabEstimate) ->
      framed " Estimate | Tab -> Editor " $ do
        estimateView
        viewKey <- keyEv (V.KChar 'v') []
        nextTabKey <- keyEv (V.KChar '\t') []
        quitKey <- keyEv (V.KChar 'q') []
        pure $
          leftmost
            [ EMsgToggleView <$ viewKey
            , EMsgNextTab <$ nextTabKey
            , EMsgQuit <$ quitKey
            ]
  grout (fixed 1) $ text (current statusDyn)
  pure msg
 where
  headerLine =
    "uncertain-gantt: " <> title <> case view of
      ModeSplit -> "  [split view]"
      ModeTabs -> "  [tab view]"
  framed name = boxTitle (pure TextAlignment_Left) (pure singleBoxStyle) (pure name)
  editor = editorPane docDyn selDyn
  estimateView = text (current estimateTextDyn)

{- | The editor: a task table with a vocabulary strip, panels for
resources and durations, and forms for adding/editing (see
tui/DESIGN.md). Adding, editing or opening a panel swaps the workflow
step; Esc (or submitting) returns.
-}
editorPane ::
  forall t m.
  (Vty t m, Adjustable t m) =>
  Dynamic t Doc ->
  Dynamic t Int ->
  m (Event t EditorMsg)
editorPane docDyn selDyn = switchDyn <$> workflow tasksStep
 where
  tasksStep :: Workflow t m (Event t EditorMsg)
  tasksStep = Workflow $ do
    let rowsDyn = View.taskRows <$> docDyn
    widthDyn <- displayWidth
    col $ do
      let stripOf label items = text . current $ View.stripLine label <$> (items <$> docDyn) <*> widthDyn
      grout (fixed 1) $
        stripOf "Resources [R]" $ \doc ->
          [View.resourceRowName r <> " ×" <> showText (View.resourceRowCapacity r) | r <- View.resourceRows doc]
      grout (fixed 1) $
        stripOf "Durations [D]" $ \doc ->
          [View.durationRowName r <> " " <> View.durationRowDefinition r | r <- View.durationRows doc]
      grout (fixed 1) $ text (current (View.separatorLine <$> widthDyn))
      grout flex $ do
        tableWidth <- displayWidth
        tableHeight <- displayHeight
        text . current $ View.renderTaskTable <$> tableWidth <*> tableHeight <*> selDyn <*> rowsDyn
      grout (fixed 1) $ text (current (View.separatorLine <$> widthDyn))
      grout (fixed 1) $ text (current (View.taskDetailLine <$> selDyn <*> rowsDyn))
    upKeys <- traverse (`keyEv` []) [V.KUp, V.KChar 'k']
    downKeys <- traverse (`keyEv` []) [V.KDown, V.KChar 'j']
    addKeys <- traverse (`keyEv` []) [V.KChar 'a', V.KChar 't']
    editKey <- keyEv V.KEnter []
    deleteKey <- keyEv (V.KChar 'x') []
    resourcesKey <- keyEv (V.KChar 'R') []
    durationsKeys <- traverse (`keyEv` []) [V.KChar 'D', V.KChar 'u']
    viewKey <- keyEv (V.KChar 'v') []
    nextTabKey <- keyEv (V.KChar '\t') []
    quitKey <- keyEv (V.KChar 'q') []
    let moveEv = leftmost [(-1) <$ leftmost upKeys, 1 <$ leftmost downKeys]
    (deleteOpEv, warnEv) <-
      guardedDelete
        (tag (current ((,) <$> rowsDyn <*> selDyn)) deleteKey)
        (leftmost [() <$ moveEv, () <$ leftmost addKeys, () <$ editKey])
        ( \(rows, sel) -> case drop sel rows of
            (selRow : _) -> Just (View.taskRowIndex selRow, View.taskRowName selRow, View.taskRowDependents selRow)
            [] -> Nothing
        )
        (\name n -> name <> " is a dependency of " <> countTasks n <> " — press x again to delete")
    let msg =
          leftmost
            [ EMsgMove <$> moveEv
            , EMsgOp . OpDelete <$> deleteOpEv
            , EMsgStatus <$> warnEv
            , EMsgToggleView <$ viewKey
            , EMsgNextTab <$ nextTabKey
            , EMsgQuit <$ quitKey
            ]
        openFormEv =
          leftmost
            [ ffor (tag (current docDyn) (leftmost addKeys)) $ \doc -> (OpInsert, newTaskSpec doc)
            , fforMaybe (tag (current ((,,) <$> docDyn <*> rowsDyn <*> selDyn)) editKey) $
                \(doc, rows, sel) -> case drop sel rows of
                  (selRow : _)
                    | (element : _) <- drop (View.taskRowIndex selRow) doc ->
                        Just (OpReplace (View.taskRowIndex selRow), editSpec doc element)
                  _ -> Nothing
            ]
    pure
      ( msg
      , leftmost
          [ formStep tasksStep <$> openFormEv
          , resourcesStep <$ resourcesKey
          , durationsStep <$ leftmost durationsKeys
          ]
      )

  resourcesStep, durationsStep :: Workflow t m (Event t EditorMsg)
  resourcesStep =
    panelStep
      "Resources"
      (\doc -> [(View.resourceRowIndex r, View.resourceRowName r, View.resourceRowUsedBy r) | r <- View.resourceRows doc])
      (\w h sel doc -> View.renderResourcePanel w h sel (View.resourceRows doc))
      (\sel doc -> View.resourceDetailLine sel (View.resourceRows doc))
      newResourceSpec
      resourcesStep
  durationsStep =
    panelStep
      "Durations"
      (\doc -> [(View.durationRowIndex r, View.durationRowName r, View.durationRowUsedBy r) | r <- View.durationRows doc])
      (\w h sel doc -> View.renderDurationPanel w h sel (View.durationRows doc))
      (\sel doc -> View.durationDetailLine sel (View.durationRows doc))
      newAliasSpec
      durationsStep

  -- \| A vocabulary management panel: browsable rows with usage info, its
  -- own selection, guarded deletes, and add/edit via the shared forms.
  panelStep ::
    Text ->
    (Doc -> [(Int, Text, [Text])]) ->
    (Int -> Int -> Int -> Doc -> Text) ->
    (Int -> Doc -> Text) ->
    FormSpec ->
    Workflow t m (Event t EditorMsg) ->
    Workflow t m (Event t EditorMsg)
  panelStep heading rowsOf renderPanel detailOf addSpec self = Workflow $ do
    let rowsDyn = rowsOf <$> docDyn
        countDyn = length <$> rowsDyn
    widthDyn <- displayWidth
    upKeys <- traverse (`keyEv` []) [V.KUp, V.KChar 'k']
    downKeys <- traverse (`keyEv` []) [V.KDown, V.KChar 'j']
    addKey <- keyEv (V.KChar 'a') []
    editKey <- keyEv V.KEnter []
    deleteKey <- keyEv (V.KChar 'x') []
    escKey <- keyEv V.KEsc []
    let moveEv = leftmost [(-1) <$ leftmost upKeys, 1 <$ leftmost downKeys]
    selDynL <-
      foldDyn ($) 0 $
        mergeWith
          (.)
          [ ffor (attach (current countDyn) moveEv) $ \(n, delta) sel -> clampSel n (sel + delta)
          , clampSel <$> updated countDyn
          ]
    col $ do
      grout (fixed 1) $ text (pure (heading <> "   (Esc to go back)"))
      grout (fixed 1) $ text (current (View.separatorLine <$> widthDyn))
      grout flex $ do
        tableWidth <- displayWidth
        tableHeight <- displayHeight
        text . current $ renderPanel <$> tableWidth <*> tableHeight <*> selDynL <*> docDyn
      grout (fixed 1) $ text (current (View.separatorLine <$> widthDyn))
      grout (fixed 2) $ text (current (detailOf <$> selDynL <*> docDyn))
      grout (fixed 1) $ text (pure "a add   Enter edit   x delete   Esc back")
    (deleteOpEv, warnEv) <-
      guardedDelete
        (tag (current ((,) <$> rowsDyn <*> selDynL)) deleteKey)
        (leftmost [() <$ moveEv, () <$ addKey, () <$ editKey])
        ( \(rows, sel) -> case drop sel rows of
            (selRow : _) -> Just selRow
            [] -> Nothing
        )
        (\name n -> name <> " is used by " <> countTasks n <> " — press x again to delete")
    let msg =
          leftmost
            [ EMsgOp . OpDelete <$> deleteOpEv
            , EMsgStatus <$> warnEv
            ]
        openFormEv =
          leftmost
            [ (OpInsert, addSpec) <$ addKey
            , fforMaybe (tag (current ((,,) <$> docDyn <*> rowsDyn <*> selDynL)) editKey) $
                \(doc, rows, sel) -> case drop sel rows of
                  ((docIndex, _, _) : _)
                    | (element : _) <- drop docIndex doc ->
                        Just (OpReplace docIndex, editSpec doc element)
                  _ -> Nothing
            ]
    pure (msg, leftmost [formStep self <$> openFormEv, tasksStep <$ escKey])

  -- \| Deleting something that is still referenced requires a second x on
  -- the same element; any other activity disarms the confirmation.
  guardedDelete ::
    Event t a ->
    Event t () ->
    (a -> Maybe (Int, Text, [Text])) ->
    (Text -> Int -> Text) ->
    m (Event t Int, Event t Text)
  guardedDelete attemptEv disarmEv selected warning = do
    rec armedDyn <-
          holdDyn Nothing $
            leftmost [Just <$> armEv, Nothing <$ deleteEv, Nothing <$ disarmEv]
        let decisionEv =
              attachWith
                ( \armed a -> do
                    (docIndex, name, referencedBy) <- selected a
                    if null referencedBy || armed == Just docIndex
                      then Just (Left docIndex)
                      else Just (Right (docIndex, warning name (length referencedBy)))
                )
                (current armedDyn)
                attemptEv
            deleteEv = fforMaybe decisionEv $ \case Just (Left i) -> Just i; _ -> Nothing
            armEv = fforMaybe decisionEv $ \case Just (Right (i, _)) -> Just i; _ -> Nothing
            warnEv = fforMaybe decisionEv $ \case Just (Right (_, w)) -> Just w; _ -> Nothing
    pure (deleteEv, warnEv)

  countTasks :: Int -> Text
  countTasks 1 = "1 task"
  countTasks n = showText n <> " tasks"

  formStep :: Workflow t m (Event t EditorMsg) -> (Element -> DocOp, FormSpec) -> Workflow t m (Event t EditorMsg)
  formStep back (mkOp, spec) = Workflow $ do
    rec result <- form (formTitle spec) errorDyn (formFields spec)
        let parsedEv = formParse spec <$> tag (current (formValues result)) (formSubmit result)
            okEv = fforMaybe parsedEv (either (const Nothing) Just)
            errEv = fforMaybe parsedEv (either Just (const Nothing))
        errorDyn <- holdDyn "" errEv
    pure (EMsgOp . mkOp <$> okEv, back <$ leftmost [() <$ okEv, formCancel result])

estimateText :: [Text] -> Bool -> Maybe (Either Text Report) -> Text
estimateText issues stale lastReport =
  Text.intercalate "\n" (staleLine : issueLines <> [""] <> bodyLines)
 where
  staleLine
    | stale = "(project changed - press C-r or F5 to re-estimate)"
    | otherwise = "C-r or F5 to re-estimate"
  issueLines = case issues of
    [] -> []
    _ ->
      ("!! " <> showText (length issues) <> " issue" <> (if length issues == 1 then "" else "s") <> " — only usable tasks are estimated:")
        : fmap (" ! " <>) issues
  bodyLines = case lastReport of
    Nothing -> ["No estimate yet. Press C-r or F5."]
    Just (Left err) -> ["Last estimate failed: " <> err]
    Just (Right report) -> Text.lines (renderReport report)
