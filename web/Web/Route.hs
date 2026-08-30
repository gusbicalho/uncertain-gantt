{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | URL structure. The open document lives in the path, which is what lets
several documents be edited at once: Hyperbole actions POST to the current
URL and both transports derive @request.path@ from WAI's @rawPathInfo@, so
every @update@ handler can recover the document it was fired against
(see "Web.State".'Web.State.currentKey').

Because @rawPathInfo@ is /not/ percent-decoded and Hyperbole's 'routePath'
does not encode, segments are escaped here on both sides and round-trip
exactly.

> /                          the launch document, or the file list
> /files                     pick a project file
> /edit/<file>               a file's first project
> /edit/<file>/<project>     one project of a multi-project file
-}
module Web.Route (
  DocKey (..),
  AppRoute (..),
  docKeyTitle,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.URI (escapeURIString, isUnreserved, unEscapeString)
import Web.Hyperbole (Route (..))
import Web.Hyperbole.Data.URI (Path (Path), Segment)

{- | Which document: a file name relative to the served directory, and the
project entry within it (TOML files hold many; @.ug@ scripts hold one, and
use 'Nothing').

'dkFile' is always a bare file name — 'Web.State.resolveKey' rejects
anything that is not in the served directory's listing, which is what
keeps a crafted URL from escaping the directory.
-}
data DocKey = DocKey
  { dkFile :: FilePath
  , dkProject :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  -- JSON so a 'DocKey' can travel inside a 'Web.Hyperbole.ViewAction'
  -- (the file strip's close button carries one).
  deriving anyclass (ToJSON, FromJSON)

data AppRoute
  = RouteIndex
  | RouteFiles
  | RouteEdit DocKey
  deriving stock (Eq, Show)

-- | Hand-written rather than generic: segments need escaping.
instance Route AppRoute where
  baseRoute = Just RouteIndex

  matchRoute (Path segments) = case segments of
    [] -> Just RouteIndex
    ["files"] -> Just RouteFiles
    ["edit", file] -> Just (RouteEdit (DocKey (unescape file) Nothing))
    ["edit", file, project] ->
      Just (RouteEdit (DocKey (unescape file) (Just (Text.pack (unescape project)))))
    _ -> Nothing

  routePath = \case
    RouteIndex -> Path []
    RouteFiles -> Path ["files"]
    RouteEdit (DocKey file mbProject) ->
      Path $ ["edit", escape file] <> foldMap (\p -> [escape (Text.unpack p)]) mbProject

escape :: FilePath -> Segment
escape = Text.pack . escapeURIString isUnreserved

unescape :: Segment -> FilePath
unescape = unEscapeString . Text.unpack

-- | How a document is named in the header, the file strip and the tab title.
docKeyTitle :: DocKey -> Text
docKeyTitle (DocKey file mbProject) =
  Text.pack file <> foldMap (" · " <>) mbProject
