{-# LANGUAGE NoFieldSelectors #-}

{- | The Surface and Capability for the document store, as interfaces —
records of functions, no 'GHC.Conc.TVar' or 'IO' in sight. "Web.State" is
the one concrete adapter implementing them today, over a 'GHC.Conc.TVar';
kept separate (per Fowler's Separated Interface — the interface belongs
with the core, not the adapter) so a different implementation — a test
mock, say, backed by nothing but an in-memory value — can satisfy the
same types without pulling in persistence or concurrency at all. Their
constructors are exported for exactly that: building an adapter (real or
fake) is the one thing outside code is allowed to do with these types.

'NoFieldSelectors' means every field below is dot-only
('OverloadedRecordDot' provides the '.' syntax) — there is no ordinary
prefix function to fall back to, matching how "Web.Hyperbole" treats its
own 'Web.Hyperbole.Request'.
-}
module Web.Capability (
  DocsSurface (..),
  DocHandle (..),
) where

import Data.Text (Text)
import Editor.Doc (Doc)
import Effectful (Eff)
import Web.Docs (DocState, ServerState)
import Web.Route (DocKey)

{- | A capability over one open document, fixed to a single 'DocKey' at
mint time (by 'DocsSurface's @docsOpen@, or "Web.State"'s
@requireDocHandle@). A handler holding one of these can read, mutate and
save that document and no other — there is no method that takes a
'DocKey' argument, so there is no way to point it at a sibling document
by accident.
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
  , dhArmClose :: Eff es ()
  {- ^ Arm the two-click close guard on this document. Closing a dirty
  document drops its undo stack, so the file strip arms first and closes
  on the second click.
  -}
  , dhClose :: Eff es ()
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
  , docsLookup :: DocKey -> Eff es (Maybe (DocState, DocHandle es))
  {- ^ Mint a handle for an /already open/ document, without the
  load-from-disk fallback of 'docsOpen' — 'Nothing' simply means "not
  open". This is how the file strip acts on a document other than the
  one its request is about: it lists every open document and offers a
  close button on each, so it needs a capability per row rather than an
  operation taking a key.
  -}
  }
