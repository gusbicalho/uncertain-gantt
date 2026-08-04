{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Choosing what to edit: the 'FileBar' strip of open documents (shown on
every page) and the file browser at @\/files@.
-}
module Web.Files (
  FileBar (..),
  {- | Exported so the editor's views can 'Web.Hyperbole.trigger' a
  refresh when a document's dirty flag changes.
  -}
  Action (FileBarRefresh, CloseFile),
  fileBarView,
  filesPage,
) where

import Control.Monad (when)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Editor.Persistence (LoadedDoc (loadedProjects), loadDocument)
import Effectful (IOE, liftIO)
import Effectful.Reader.Static (Reader)
import System.FilePath ((</>))
import Web.Capability (
  DocHandle (dhArmClose, dhClose),
  DocsSurface (docsLookup, docsProjectFiles, docsSnapshot),
 )
import Web.Core (CloseAction (ArmClose), closeAction)
import Web.Docs (DocState (dsCloseArmed, dsDirty), ServerState (ssOpen, ssOrder, ssRoot))
import Web.Hyperbole
import Web.Route (AppRoute (RouteEdit, RouteFiles), DocKey (DocKey), docKeyTitle)
import Web.State (
  Adapters,
  activeKey,
  docsSurface,
 )
import Web.Styles (styles)

data FileBar = FileBar
  deriving stock (Generic)
  deriving anyclass (ViewId)

instance (Reader Adapters :> es, IOE :> es) => HyperView FileBar es where
  data Action FileBar = FileBarRefresh | CloseFile DocKey
    deriving stock (Generic)
    deriving anyclass (ViewAction)

  update FileBarRefresh = fileBar
  -- Whether a click arms or closes is 'Web.Core.closeAction'; this only
  -- carries it out. The strip is the one place a request acts on a
  -- document other than its own — it lists every open document — so it
  -- mints a capability for the row that was clicked ('docsLookup', which
  -- does not load from disk) rather than passing a key to the surface.
  update (CloseFile key) = do
    surface <- docsSurface
    entry <- surface.docsLookup key
    case entry of
      Just (doc, handle) | ArmClose <- closeAction doc -> do
        handle.dhArmClose
        fileBar
      _ -> do
        -- 'Nothing' means a concurrent request already closed it; either
        -- way the page may still be showing it, so redirect regardless.
        mapM_ (\(_, handle) -> handle.dhClose) entry
        active <- activeKey
        if active == Just key
          then redirect (routeUri RouteFiles)
          else fileBar

fileBar :: (Hyperbole :> es, Reader Adapters :> es, IOE :> es) => Eff es (View FileBar ())
fileBar = do
  surface <- docsSurface
  server <- surface.docsSnapshot
  active <- activeKey
  pure (fileBarView active server)

fileBarView :: Maybe DocKey -> ServerState -> View FileBar ()
fileBarView active server = el @ att "class" "filebar" $ do
  el @ att "class" "filebar-label" $ text "Open:"
  case ssOrder server of
    [] -> el @ att "class" "filebar-label" $ text "(nothing yet)"
    keys -> mapM_ item keys
  route RouteFiles (text "Files…") @ att "class" "btn-link"
 where
  item key = el @ att "class" (if active == Just key then "filebar-item active" else "filebar-item") $ do
    route (RouteEdit key) (text (docKeyTitle key)) @ att "class" "filebar-link"
    when dirty $ el @ att "class" "filebar-mark" $ text "•"
    button (CloseFile key) (text (if armed then "discard?" else "×"))
      @ att "class" (if armed then "btn-link btn-close armed" else "btn-link btn-close")
   where
    doc = Map.lookup key (ssOpen server)
    dirty = maybe False dsDirty doc
    armed = maybe False dsCloseArmed doc

-- | A file in the served directory, with the projects it holds.
data FileEntry = FileEntry FilePath (Either Text [Text])

filesPage :: (Reader Adapters :> es, IOE :> es) => Page es '[FileBar]
filesPage = do
  surface <- docsSurface
  server <- surface.docsSnapshot
  files <- surface.docsProjectFiles
  entries <- traverse (describe server) files
  pure $ do
    styles
    el @ att "class" "app" $ do
      hyper FileBar (fileBarView Nothing server)
      el @ att "class" "title" $ text ("Project files in " <> Text.pack (ssRoot server))
      case entries of
        [] -> el @ att "class" "empty" $ text "No .toml or .ug files in this directory."
        rows -> el @ att "class" "filelist" $ mapM_ fileRow rows

{- | Read each file just for its project names. Files that don't parse are
listed with their error rather than hidden.
-}
describe :: (IOE :> es) => ServerState -> FilePath -> Eff es FileEntry
describe server file = do
  loaded <- liftIO (loadDocument (ssRoot server </> file) Nothing)
  pure . FileEntry file $ fmap loadedProjects loaded

fileRow :: FileEntry -> View c ()
fileRow (FileEntry file projects) = el @ att "class" "filelist-row" $ do
  route (RouteEdit (DocKey file Nothing)) (text (Text.pack file)) @ att "class" "filelist-link"
  case projects of
    Left err -> el @ att "class" "page-error" $ text err
    -- One project, or a .ug script: the file name is the only link needed.
    Right names
      | length names <= 1 -> none
      | otherwise ->
          el @ att "class" "filelist-projects" $
            sequence_ (List.intersperse (text " · ") (projectLink <$> names))
 where
  projectLink project =
    route (RouteEdit (DocKey file (Just project))) (text project) @ att "class" "project-link"
