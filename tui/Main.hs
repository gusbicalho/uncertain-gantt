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

import Tui.Doc (
  Doc,
  DocOp (OpDelete, OpInsert, OpReplace),
  DocProject,
  FormSpec (formFields, formParse, formTitle),
  applyOp,
  docProject,
  editSpec,
  elementLabel,
  fromStatements,
  newAliasSpec,
  newResourceSpec,
  newTaskSpec,
  toStatements,
 )
import Tui.Estimate (Report, defaultRuns, renderReport, runReport)
import Tui.Widgets (FormResult (formCancel, formSubmit, formValues), Vty, form, keyEv, selectList)
import UncertainGantt.Script.Parser (parseScript)
import UncertainGantt.Script.Render (renderDeclarations)

main :: IO ()
main =
  getArgs >>= \case
    [] -> start "project.ug"
    [path] -> start path
    _ -> die "usage: uncertain-gantt-tui [FILE]"

start :: FilePath -> IO ()
start path = do
  exists <- doesFileExist path
  (doc0, dropped) <-
    if not exists
      then pure ([], 0)
      else do
        contents <- readFile path
        case parseScript contents of
          Left (err, _) -> die ("Failed to parse " <> path <> ":\n" <> err)
          Right statements -> pure (fromStatements statements)
  runApp path doc0 dropped

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

runApp :: FilePath -> Doc -> Int -> IO ()
runApp path doc0 dropped = mainWidget def $ initManager_ $ do
  rec docDyn <- foldDyn applyOp doc0 docOpEv
      let docLen = length <$> docDyn
      selDyn <-
        foldDyn ($) 0 $
          mergeWith
            (.)
            [ ffor (attach (current docLen) moveEv) $ \(n, delta) sel -> clampSel n (sel + delta)
            , clampSel <$> updated docLen
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
      let estimateTextDyn = estimateText <$> (docProject <$> docDyn) <*> staleDyn <*> reportDyn

      saveKeyEv <- keyEv (V.KChar 's') [V.MCtrl]
      savedEv <- performEvent $
        ffor (tag (current docDyn) saveKeyEv) $ \doc -> liftIO $ do
          Text.IO.writeFile path (renderDeclarations (toStatements doc))
          pure $ "Saved " <> Text.pack path

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
            , savedEv
            , hints <$ docOpEv
            ]

      paneEvs <- networkView $
        ffor ((,) <$> viewDyn <*> tabDyn) $ \cfg ->
          renderPanes cfg (Text.pack path) docDyn selDyn estimateTextDyn statusDyn
      paneMsgEv <- switchHold never paneEvs
      let docOpEv = fforMaybe paneMsgEv $ \case EMsgOp op -> Just op; _ -> Nothing
          moveEv = fforMaybe paneMsgEv $ \case EMsgMove d -> Just d; _ -> Nothing
          toggleViewEv = fforMaybe paneMsgEv $ \case EMsgToggleView -> Just (); _ -> Nothing
          nextTabEv = fforMaybe paneMsgEv $ \case EMsgNextTab -> Just (); _ -> Nothing
          paneQuitEv = fforMaybe paneMsgEv $ \case EMsgQuit -> Just (); _ -> Nothing
  ctrlC <- keyEv (V.KChar 'c') [V.MCtrl]
  pure $ leftmost [ctrlC, quitOkEv]
 where
  clampSel n sel = max 0 (min (n - 1) sel)
  initialStatus
    | dropped > 0 = "[" <> Text.pack (show dropped) <> " print/run statements ignored] " <> hints
    | otherwise = hints

hints :: Text
hints = "j/k move | Enter edit | t/r/u add | x del | C-r estimate | C-s save | v view | C-q quit"

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

{- | The editor: a browsable element list; adding or editing swaps in a
form, then returns to the list.
-}
editorPane ::
  forall t m.
  (Vty t m, Adjustable t m) =>
  Dynamic t Doc ->
  Dynamic t Int ->
  m (Event t EditorMsg)
editorPane docDyn selDyn = switchDyn <$> workflow browseStep
 where
  browseStep :: Workflow t m (Event t EditorMsg)
  browseStep = Workflow $ do
    selectList (fmap elementLabel <$> docDyn) selDyn
    upKeys <- traverse (`keyEv` []) [V.KUp, V.KChar 'k']
    downKeys <- traverse (`keyEv` []) [V.KDown, V.KChar 'j']
    addTaskKey <- keyEv (V.KChar 't') []
    addResourceKey <- keyEv (V.KChar 'r') []
    addAliasKey <- keyEv (V.KChar 'u') []
    editKey <- keyEv V.KEnter []
    deleteKey <- keyEv (V.KChar 'x') []
    viewKey <- keyEv (V.KChar 'v') []
    nextTabKey <- keyEv (V.KChar '\t') []
    quitKey <- keyEv (V.KChar 'q') []
    let msg =
          leftmost
            [ EMsgMove (-1) <$ leftmost upKeys
            , EMsgMove 1 <$ leftmost downKeys
            , EMsgOp . OpDelete <$> tag (current selDyn) deleteKey
            , EMsgToggleView <$ viewKey
            , EMsgNextTab <$ nextTabKey
            , EMsgQuit <$ quitKey
            ]
        openFormEv =
          leftmost
            [ ffor (tag (current docDyn) addTaskKey) $ \doc -> (Nothing, newTaskSpec doc)
            , (Nothing, newResourceSpec) <$ addResourceKey
            , (Nothing, newAliasSpec) <$ addAliasKey
            , fforMaybe (tag (current ((,) <$> docDyn <*> selDyn)) editKey) $ \(doc, sel) ->
                case drop sel doc of
                  (element : _) -> Just (Just sel, editSpec doc element)
                  [] -> Nothing
            ]
    pure (msg, formStep <$> openFormEv)

  formStep :: (Maybe Int, FormSpec) -> Workflow t m (Event t EditorMsg)
  formStep (target, spec) = Workflow $ do
    rec result <- form (formTitle spec) errorDyn (formFields spec)
        let parsedEv = formParse spec <$> tag (current (formValues result)) (formSubmit result)
            okEv = fforMaybe parsedEv (either (const Nothing) Just)
            errEv = fforMaybe parsedEv (either Just (const Nothing))
        errorDyn <- holdDyn "" errEv
    let opEv = maybe OpInsert OpReplace target <$> okEv
    pure (EMsgOp <$> opEv, browseStep <$ leftmost [() <$ okEv, formCancel result])

estimateText :: Either Text DocProject -> Bool -> Maybe (Either Text Report) -> Text
estimateText build stale lastReport =
  Text.intercalate "\n" (statusLines <> [""] <> bodyLines)
 where
  statusLines = case build of
    Left err -> ["!! " <> err]
    Right _
      | stale -> ["(project changed - press C-r or F5 to re-estimate)"]
      | otherwise -> ["C-r or F5 to re-estimate"]
  bodyLines = case lastReport of
    Nothing -> ["No estimate yet. Press C-r or F5."]
    Just (Left err) -> ["Last estimate failed: " <> err]
    Just (Right report) -> Text.lines (renderReport report)
