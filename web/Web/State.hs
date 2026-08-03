{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
-- Hyperbole's Request has NoFieldSelectors; record dot is how you read it.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- | The server side of "what is open": the one concrete adapter — over a
'TVar' — implementing the 'DocsSurface'/'DocHandle' interfaces from
"Web.Capability", plus the wiring to reach them from a request.

The shape follows Ports-and-Adapters/capability discipline:

* The 'TVar' is the one Resource. It is built once, in 'newAdapters', and
  never leaves this module — nothing outside can pattern-match it out or
  reach it ambiently.
* 'DocsSurface'/'DocHandle' (defined in "Web.Capability", not here) are
  the Surface and the Capability it hands out; this module is just one
  way of implementing them, over a 'TVar'. Kept separate so another
  implementation (a test mock, say) could satisfy the same interfaces
  without any of the machinery below.
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
  Adapters,
  newAdapters,
  docsSurface,
  currentKey,
  activeKey,
  requireDocHandle,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
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
import Web.Capability (DocHandle (..), DocsSurface (..))
import Web.Docs (DocState (..), ServerState (..))
import Web.Hyperbole
import Web.Route (AppRoute (RouteEdit), DocKey (dkFile, dkProject))

undoLimit :: Int
undoLimit = 100

docPath :: ServerState -> DocKey -> FilePath
docPath server key = ssRoot server </> dkFile key

{- | The one record of adapters: built once by 'newAdapters' in "Main",
threaded to every handler as a single @'Reader' 'Adapters'@ effect. Its
constructor isn't exported, so nothing outside this module can fabricate
one bypassing the real, 'TVar'-backed implementation.
-}
newtype Adapters = Adapters
  { getDocs :: forall es. (IOE :> es) => Eff es (DocsSurface es)
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
  pure Adapters{getDocs = pure (mkDocsSurface tvar)}

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

{- | Project 'Adapters'' one adapter, instantiated at the caller's own
effect row, so callers get a concrete 'DocsSurface' to use normally:

@
surface <- 'docsSurface'
surface.docsSnapshot
@

Two notes for anyone touching this. First, the field is read with
ordinary prefix application (@getDocs adapters@), not @adapters.getDocs@:
'HasField' has no instance for a field whose /declared/ type is itself
quantified, so record-dot syntax cannot project 'getDocs' at all — that
is a property of the field, independent of whether the use site is
inferring or checking. The auto-generated selector /function/ is merely
top-level-polymorphic, which instantiates by ordinary application.

Second, the field's result is wrapped in 'Eff' (@Eff es (DocsSurface
es)@, not a bare @DocsSurface es@) so that this projection is a plain
monadic bind at every call site, rather than a CPS-style
@withDocs (\\surface -> ...)@ that would force each caller to nest its
whole body inside a lambda.
-}
docsSurface :: (Reader Adapters :> es, IOE :> es) => Eff es (DocsSurface es)
docsSurface = do
  adapters <- ask @Adapters
  getDocs adapters

{- | The handle for the document an action was fired against. Unlike
@docsOpen@ this cannot recover from a bad key: an action has no view to
render instead.
-}
requireDocHandle ::
  (Hyperbole :> es, Reader Adapters :> es, IOE :> es) =>
  Eff es (DocState, DocHandle es)
requireDocHandle = do
  surface <- docsSurface
  key <- currentKey
  surface.docsOpen key >>= \case
    Nothing -> notFound
    Just (Left err) -> respondErrorView "Could not read document" (el (text err))
    Just (Right (_, doc0, handle)) -> pure (doc0, handle)
