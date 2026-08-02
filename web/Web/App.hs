{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Route wiring: which URL shows which page (see "Web.Route" for the URL
shapes). Everything else lives in "Web.Editor" (one document's editor),
"Web.Files" (picking a document) and "Web.State" (what is open).
-}
module Web.App (app) where

import Data.Text (Text)
import Effectful (IOE)
import Effectful.Reader.Dynamic (Reader, ask)
import Web.Editor (editorPage)
import Web.Files (FileBar (FileBar), fileBarView, filesPage)
import Web.Hyperbole
import Web.Route (AppRoute (RouteEdit, RouteFiles, RouteIndex), DocKey, docKeyTitle)
import Web.State (
  Adapters,
  ServerState (ssStartup),
  docs,
  docsOpen,
  docsSnapshot,
 )
import Web.Styles (styles)

app :: (Hyperbole :> es, Reader Adapters :> es, IOE :> es) => Eff es Response
app = routeRequest router

router :: (Hyperbole :> es, Reader Adapters :> es, IOE :> es) => AppRoute -> Eff es Response
router = \case
  -- Launched against a file, @/@ lands in its editor, as it always has;
  -- launched against a directory, it lands in the file browser.
  RouteIndex -> do
    adapters <- ask
    server <- docsSnapshot (docs adapters)
    redirect . routeUri $ maybe RouteFiles RouteEdit (ssStartup server)
  RouteFiles -> runPage filesPage
  RouteEdit key -> do
    adapters <- ask
    docsOpen (docs adapters) key >>= \case
      Nothing -> notFound
      Just (Left err) -> runPage (errorPage key err)
      Just (Right (canonical, doc, _handle))
        -- The project was implicit in the URL; say which one it resolved to.
        | canonical /= key -> redirect (routeUri (RouteEdit canonical))
        | otherwise -> do
            server <- docsSnapshot (docs adapters)
            runPage (editorPage server canonical doc)

-- | A document we serve but could not read — a malformed file, usually.
errorPage :: (Reader Adapters :> es, IOE :> es) => DocKey -> Text -> Page es '[FileBar]
errorPage key err = do
  adapters <- ask
  server <- docsSnapshot (docs adapters)
  pure $ do
    styles
    el @ att "class" "app" $ do
      hyper FileBar (fileBarView Nothing server)
      el @ att "class" "title" $ text (docKeyTitle key)
      el @ att "class" "page-error" $ text err
