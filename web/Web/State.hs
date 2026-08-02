{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
-- Hyperbole's Request has NoFieldSelectors; record dot is how you read it.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- | Server state: the directory being served, and every document open in
it. Each open document carries its own undo stack, estimate and dirty
flag, so switching between files never discards work in progress.

The shape follows Ports-and-Adapters/capability discipline:

* The 'TVar' is the one Resource. It is built once, in 'newAdapters', and
  never leaves this module — nothing outside can pattern-match it out or
  reach it ambiently.
* 'DocsSurface' is the Surface over it: wide (every open document, every
  file in the directory), shallow (it hands out capabilities and plain
  reads, does no rendering).
* 'DocHandle' is the Capability 'DocsSurface' hands out: fixed to one
  'DocKey' at mint time, so a handler holding one cannot reach a sibling
  document by constructing the wrong key.
* 'Adapters' is the one record of adapters, built once in "Main" and
  injected as a single @'Reader' 'Adapters'@ effect — the driving
  adapters (the @HyperView@ instances) never see the 'TVar', only this.
  Its field is universally quantified over the effect row (needs
  'RankNTypes') because different @HyperView@ dispatches run in
  different concrete rows (each adds its own @Reader@\/@State@ layer per
  "Web.Hyperbole"'s own dispatch mechanism), so one 'Adapters' value has
  to serve all of them, not just the row it happened to be built in.

Which document a request concerns still comes from the URL, not from any
effect ('currentKey') — Hyperbole actions POST to the current URL, so the
route is readable in every handler.
-}
module Web.State (
  DocState (..),
  ServerState (..),
  DocsSurface (docsSnapshot, docsProjectFiles, docsOpen, docsArmClose, docsClose),
  DocHandle (dhKey, dhModify, dhApplyChange, dhSave),
  Adapters (docs),
  newAdapters,
  docPath,
  currentKey,
  activeKey,
  requireDocHandle,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Editor.Doc (Doc)
import Editor.Estimate (Report)
import Editor.Persistence (
  LoadedDoc (loadedDoc, loadedNote, loadedProject, loadedProjects),
  listProjectFiles,
  loadDocument,
  saveDocument,
 )
import Effectful (IOE, liftIO)
import Effectful.Reader.Dynamic (Reader, ask)
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import System.FilePath ((</>))
import Web.Hyperbole
import Web.Route (AppRoute (RouteEdit), DocKey (dkFile, dkProject))

-- | One open document. Everything here is per-document, nothing is shared.
data DocState = DocState
  { dsDoc :: Doc
  , dsUndo :: [Doc]
  -- ^ Snapshots before each change, newest first (capped).
  , dsEpoch :: Int
  -- ^ Bumped on every doc change; guards against stale estimate results.
  , dsEditing :: Maybe (Int, Int)
  -- ^ Row in edit mode: doc index and the field to focus.
  , dsShowResources :: Bool
  , dsShowDurations :: Bool
  , dsAliasDraft :: Maybe Text
  -- ^ Prefilled name for the duration quick-add (create-from-use).
  , dsReport :: Maybe (Either Text Report)
  , dsPrevReport :: Maybe Report
  -- ^ The previous successful report, for the delta line.
  , dsEstimating :: Bool
  , dsAuto :: Bool
  , dsStale :: Bool
  -- ^ Doc changed since the report shown was computed.
  , dsDirty :: Bool
  , dsNote :: Maybe Text
  , dsStatus :: Maybe Text
  , dsProjects :: [Text]
  -- ^ Every project in the file, for the header's picker.
  , dsCloseArmed :: Bool
  -- ^ Closing a dirty document drops its undo stack, so it takes two clicks.
  }

data ServerState = ServerState
  { ssRoot :: FilePath
  -- ^ The directory being served. Documents are named relative to it.
  , ssStartup :: Maybe DocKey
  -- ^ The document named on the command line, if any; @/@ redirects to it.
  , ssOpen :: Map DocKey DocState
  , ssOrder :: [DocKey]
  -- ^ Open order, for the file strip.
  }

undoLimit :: Int
undoLimit = 100

docPath :: ServerState -> DocKey -> FilePath
docPath server key = ssRoot server </> dkFile key

{- | A capability over one open document, fixed to a single 'DocKey' at
mint time (by 'docsOpen' or 'requireDocHandle'). A handler holding one of
these can read, mutate and save that document and no other — there is no
method that takes a 'DocKey' argument, so there is no way to point it at
a sibling document by accident. Its methods need only 'IOE': the 'TVar'
they close over was already read out of the resource once, when the
handle was minted.
-}
data DocHandle es = DocHandle
  { dhKey :: DocKey
  , dhModify :: (DocState -> DocState) -> Eff es DocState
  , dhApplyChange :: Maybe Text -> (Doc -> Doc) -> (DocState -> DocState) -> Eff es DocState
  {- ^ Commit a doc change: push an undo snapshot, bump the epoch (so a
  racing estimate result is discarded), mark stale + dirty, leave edit
  mode. The final argument runs on the resulting state (e.g. to focus a
  new row).
  -}
  , dhSave :: Eff es DocState
  }

{- | The Surface over every open (or openable) document: wide — it can name
any file in the served directory — and shallow — its own methods do no
rendering, they only read or mint a 'DocHandle'.
-}
data DocsSurface es = DocsSurface
  { docsSnapshot :: Eff es ServerState
  {- ^ A read-only snapshot — root path, every open document, open order
  — for rendering the file strip and the file browser.
  -}
  , docsProjectFiles :: Eff es [FilePath]
  {- ^ Files in the served directory, re-read from disk each time so ones
  created while the server runs show up.
  -}
  , docsOpen :: DocKey -> Eff es (Maybe (Either Text (DocKey, DocState, DocHandle es)))
  {- ^ Find or load a document by key, loading it from disk if it isn't
  open yet — which is also what lets a browser tab left open across a
  close (or a server restart) keep working, at the cost of that
  document's undo history. The key is canonicalised to the project
  actually selected, so @/edit/f.toml@ and @/edit/f.toml/<first
  project>@ mint the same document rather than two. Callers should
  redirect when the returned key differs from the one they asked for.

  'Nothing' means the file is not one we serve; 'Left' means it is, but
  could not be read.
  -}
  , docsArmClose :: DocKey -> Eff es ()
  {- ^ Arm the two-click close guard on a dirty document, by key. This
  (and 'docsClose') are the one legitimate place a request acts on a
  document other than its own: the file strip lists every open document
  and offers a close button on each. Both are no-ops if @key@ is not
  open, matching 'Data.Map.Strict.adjust'\/'Data.Map.Strict.delete'.
  -}
  , docsClose :: DocKey -> Eff es ()
  }

{- | The one record of adapters: built once by 'newAdapters' in "Main",
threaded to every handler as a single @'Reader' 'Adapters'@ effect. Its
constructor isn't exported, so nothing outside this module can fabricate
one bypassing the real, 'TVar'-backed implementation.
-}
newtype Adapters = Adapters
  { docs :: forall es. (IOE :> es) => DocsSurface es
  }

-- | Build the server's resources and wire its one adapter; call once from @main@.
newAdapters :: FilePath -> Maybe DocKey -> IO Adapters
newAdapters root startup = do
  tvar <-
    newTVarIO
      ServerState
        { ssRoot = root
        , ssStartup = startup
        , ssOpen = Map.empty
        , ssOrder = []
        }
  pure Adapters{docs = mkDocsSurface tvar}

mkDocsSurface :: (IOE :> es) => TVar ServerState -> DocsSurface es
mkDocsSurface tvar =
  DocsSurface
    { docsSnapshot = liftIO (readTVarIO tvar)
    , docsProjectFiles = do
        server <- liftIO (readTVarIO tvar)
        liftIO (listProjectFiles (ssRoot server))
    , docsOpen = \key ->
        fmap (fmap (\(canonicalKey, doc0) -> (canonicalKey, doc0, mkHandle tvar canonicalKey doc0)))
          <$> findOrLoad tvar key
    , docsArmClose = \key ->
        liftIO . atomically $ do
          server <- readTVar tvar
          writeTVar tvar server{ssOpen = Map.adjust (\d -> d{dsCloseArmed = True}) key (ssOpen server)}
    , docsClose = \key ->
        liftIO . atomically $ do
          server <- readTVar tvar
          writeTVar
            tvar
            server
              { ssOpen = Map.delete key (ssOpen server)
              , ssOrder = filter (/= key) (ssOrder server)
              }
    }

findOrLoad :: (IOE :> es) => TVar ServerState -> DocKey -> Eff es (Maybe (Either Text (DocKey, DocState)))
findOrLoad tvar key = do
  server <- liftIO (readTVarIO tvar)
  case Map.lookup key (ssOpen server) of
    Just doc -> pure (Just (Right (key, doc)))
    Nothing -> do
      known <- isServedFile server key
      if not known
        then pure Nothing
        else do
          loaded <- liftIO (loadDocument (docPath server key) (dkProject key))
          case loaded of
            Left err -> pure (Just (Left err))
            Right doc ->
              Just . Right
                <$> insertDoc tvar key{dkProject = loadedProject doc} (freshDoc doc)

{- | Guards against a crafted URL reaching outside the served directory: a
file name only counts if it is in the directory's own listing. The
document named on the command line is also allowed, so launching against
a file that does not exist yet still opens an empty project, as before.
-}
isServedFile :: (IOE :> es) => ServerState -> DocKey -> Eff es Bool
isServedFile server key
  | Just startup <- ssStartup server, dkFile startup == dkFile key = pure True
  | otherwise = elem (dkFile key) <$> liftIO (listProjectFiles (ssRoot server))

-- | Insert unless a concurrent request got there first.
insertDoc :: (IOE :> es) => TVar ServerState -> DocKey -> DocState -> Eff es (DocKey, DocState)
insertDoc tvar key doc =
  liftIO . atomically $ do
    server <- readTVar tvar
    case Map.lookup key (ssOpen server) of
      Just existing -> pure (key, existing)
      Nothing -> do
        writeTVar
          tvar
          server
            { ssOpen = Map.insert key doc (ssOpen server)
            , ssOrder = ssOrder server <> [key]
            }
        pure (key, doc)

freshDoc :: LoadedDoc -> DocState
freshDoc loaded =
  DocState
    { dsDoc = loadedDoc loaded
    , dsUndo = []
    , dsEpoch = 0
    , dsEditing = Nothing
    , dsShowResources = False
    , dsShowDurations = False
    , dsAliasDraft = Nothing
    , dsReport = Nothing
    , dsPrevReport = Nothing
    , dsEstimating = False
    , dsAuto = True
    , dsStale = True
    , dsDirty = False
    , dsNote = loadedNote loaded
    , dsStatus = Nothing
    , dsProjects = loadedProjects loaded
    , dsCloseArmed = False
    }

{- | @doc0@ is the snapshot read when the handle was minted: the fallback
if the document is concurrently closed before a mutation lands.
-}
mkHandle :: (IOE :> es) => TVar ServerState -> DocKey -> DocState -> DocHandle es
mkHandle tvar key doc0 =
  DocHandle
    { dhKey = key
    , dhModify = modifyWithFallback tvar key doc0
    , dhApplyChange = \status f extra ->
        modifyWithFallback tvar key doc0 $ \d ->
          extra
            d
              { dsDoc = f (dsDoc d)
              , dsUndo = take undoLimit (dsDoc d : dsUndo d)
              , dsEpoch = dsEpoch d + 1
              , dsEditing = Nothing
              , dsAliasDraft = Nothing
              , dsStale = True
              , dsDirty = True
              , dsStatus = status
              , -- An armed close must not survive a new edit, or one click
                -- would discard work the user added after arming it.
                dsCloseArmed = False
              }
    , dhSave = do
        server <- liftIO (readTVarIO tvar)
        result <- liftIO (saveDocument (docPath server key) (dkProject key) (dsDoc doc0))
        modifyWithFallback tvar key doc0 $ \d -> case result of
          Left err -> d{dsStatus = Just err}
          Right msg -> d{dsDirty = False, dsStatus = Just msg}
    }

modifyWithFallback ::
  (IOE :> es) =>
  TVar ServerState ->
  DocKey ->
  DocState ->
  (DocState -> DocState) ->
  Eff es DocState
modifyWithFallback tvar key fallback f =
  liftIO . atomically $ do
    server <- readTVar tvar
    let doc = f (Map.findWithDefault fallback key (ssOpen server))
    writeTVar tvar server{ssOpen = Map.insert key doc (ssOpen server)}
    pure doc

-- | Which document this request concerns. Only valid on an edit route.
currentKey :: (Hyperbole :> es) => Eff es DocKey
currentKey =
  activeKey >>= \case
    Just key -> pure key
    Nothing -> notFound

{- | Like 'currentKey', but for views that also render off an edit route —
the file strip highlights the open document and appears on every page.
-}
activeKey :: (Hyperbole :> es) => Eff es (Maybe DocKey)
activeKey = do
  req <- request
  pure $ case matchRoute req.path of
    Just (RouteEdit key) -> Just key
    _ -> Nothing

{- | The handle for the document an action was fired against. Unlike
'docsOpen' this cannot recover from a bad key: an action has no view to
render instead.
-}
requireDocHandle ::
  (Hyperbole :> es, Reader Adapters :> es, IOE :> es) =>
  Eff es (DocState, DocHandle es)
requireDocHandle = do
  adapters <- ask
  key <- currentKey
  docsOpen (docs adapters) key >>= \case
    Nothing -> notFound
    Just (Left err) -> respondErrorView "Could not read document" (el (text err))
    Just (Right (_, doc0, handle)) -> pure (doc0, handle)
