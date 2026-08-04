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
import Web.Capability (DocsSurface (docsArmClose, docsClose, docsProjectFiles, docsSnapshot))
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
  -- Closing drops that document's undo stack, so a dirty one takes two
  -- clicks (the TUI guards deletes the same way). This is the one place
  -- a request legitimately acts on a document other than its own — the
  -- strip lists every open document — so it goes through the surface's
  -- two named, single-purpose methods below rather than a capability
  -- that would have to expose an arbitrary mutator over an arbitrary key.
  update (CloseFile key) = do
    surface <- docsSurface
    server <- surface.docsSnapshot
    case Map.lookup key (ssOpen server) of
      Just doc | dsDirty doc && not (dsCloseArmed doc) -> do
        surface.docsArmClose key
        fileBar
      _ -> do
        surface.docsClose key
        -- The page is still showing what was just closed; leave it.
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
