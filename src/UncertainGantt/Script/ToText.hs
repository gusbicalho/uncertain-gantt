module UncertainGantt.Script.ToText (ToText (..), showText) where

import Data.Text (Text)
import Data.Text qualified as Text
import Symbolize (Symbol)
import Symbolize qualified

class ToText a where
  toText :: a -> Text.Text
  toText = Text.pack . toString

  toString :: a -> String
  toString = Text.unpack . toText

instance ToText Symbol where
  toText = Symbolize.unintern
  toString = Symbolize.unintern

instance ToText String where
  toText = Text.pack
  toString = id

instance ToText Text where
  toText = id
  toString = Text.unpack

showText :: (Show a) => a -> Text
showText = toText . show
