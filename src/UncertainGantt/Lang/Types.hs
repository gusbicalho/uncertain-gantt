{-# LANGUAGE DerivingStrategies #-}

{- | The vocabulary of project definitions: resources, duration
distributions and task descriptions. This is the language every frontend
speaks — script statements and TUI forms alike — independent of any
concrete syntax.
-}
module UncertainGantt.Lang.Types (
  Resource (..),
  unResource,
  DurationAlias (..),
  unDurationAlias,
  DurationD (..),
  TaskDescription (..),
  ResourceDescription (..),
) where

import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Symbolize (Symbol)
import UncertainGantt.Task (TaskName)
import UncertainGantt.ToText (ToText)

newtype Resource = Resource Symbol
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToText)

unResource :: Resource -> Symbol
unResource (Resource r) = r

newtype DurationAlias = DurationAlias Symbol
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToText)

unDurationAlias :: DurationAlias -> Symbol
unDurationAlias (DurationAlias a) = a

data DurationD
  = UniformD Word Word
  | NormalD Double Double
  | LogNormalD Double Double
  deriving stock (Eq, Ord, Show, Generic)

data TaskDescription = TaskDescription TaskName Text Resource (Either DurationAlias DurationD) [TaskName]
  deriving stock (Eq, Ord, Show)

data ResourceDescription = ResourceDescription Resource Word
  deriving stock (Eq, Ord, Show)
