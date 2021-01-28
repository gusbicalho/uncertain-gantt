{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UncertainGantt.Task (
  TaskName (..),
  unTaskName,
  Task (..),
) where

import Data.Set (Set)
import Data.String (IsString)

newtype TaskName = TaskName String
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

unTaskName :: TaskName -> String
unTaskName (TaskName s) = s

data Task r d = Task
  { taskName :: TaskName
  , description :: String
  , resource :: r
  , duration :: d
  , dependencies :: Set TaskName
  }
  deriving stock (Eq, Ord, Show)
