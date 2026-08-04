{-# LANGUAGE DerivingStrategies #-}

{- | Web-specific policy: decisions that are neither rendering nor
storage, and that hold however either is implemented.

Everything here is pure — no capability argument, no effect row, no
'Web.Hyperbole' — so it is the part of the web layer whose behaviour can
be stated and checked without a running server. That is the point of
separating it: the driving adapters in "Web.Files" and "Web.Editor"
decide *what to do* by calling into here, and are left with only the
doing.

(These modules live in an executable's @hs-source-dirs@, so @cabal test@
still cannot reach them — see the refactoring list in ARCHITECTURE.md.
Being pure is what would make them trivially testable once it can.)
-}
module Web.Core (
  CloseAction (..),
  closeAction,
) where

import Web.Docs (DocState (dsCloseArmed, dsDirty))

-- | What a click on a document's close button should do.
data CloseAction
  = {- | The document has unsaved work and has not been armed yet. Closing
    would drop its undo stack, so the first click only arms the guard.
    -}
    ArmClose
  | -- | Clean, or already armed by a previous click: close it.
    CloseNow
  deriving stock (Eq, Show)

{- | The two-click guard on closing a dirty document. The web editor
deliberately differs from the TUI here (see web/DESIGN.md): deletes are
undoable and so happen immediately, but a close drops the undo stack
outright, which is the one action undo cannot cover.
-}
closeAction :: DocState -> CloseAction
closeAction doc
  | dsDirty doc && not (dsCloseArmed doc) = ArmClose
  | otherwise = CloseNow
