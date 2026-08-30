-- | A UI-framework-agnostic description of one add/edit form field.
module Editor.FormField (
  FormField (..),
) where

import Data.Text (Text)

data FormField = FormField
  { fieldLabel :: Text
  , fieldInitial :: Text
  , fieldCompletions :: [Text]
  {- ^ Candidates offered while the field is focused; empty for free-form
  fields. For list-valued fields (comma-separated), completion applies
  to one item at a time.
  -}
  }
