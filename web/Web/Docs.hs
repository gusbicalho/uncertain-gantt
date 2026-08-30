{- | Domain data shared by the document store's Surface and Capability
(see "Web.Capability") and its concrete adapter (see "Web.State"): what
an open document looks like, and what the server as a whole is serving.
Plain data, ordinary field selectors — unlike "Web.Capability", nothing
here needs to stay dot-only, since it's read throughout the view-rendering
code in "Web.Editor" and "Web.Files".
-}
module Web.Docs (
  DocState (..),
  ServerState (..),
) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Editor.Doc (Doc)
import Editor.Estimate (Report)
import Web.Route (DocKey)

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
