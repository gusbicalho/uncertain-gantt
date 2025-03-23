{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UncertainGantt.Task (
  TaskName (..),
  unTaskName,
  Task (..),
) where

import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Symbolize (Symbol)
import UncertainGantt.Script.ToText (ToText)

newtype TaskName = TaskName Symbol
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToText)

unTaskName :: TaskName -> Symbol
unTaskName (TaskName s) = s

data Task r d = Task
  { taskName :: TaskName
  , description :: Text
  , resource :: r
  , duration :: d
  , dependencies :: Set TaskName
  }
  deriving stock (Eq, Ord, Show)
