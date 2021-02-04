{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module UncertainGantt.Script.Types where

import Data.String (IsString)
import qualified Text.Megaparsec as P
import UncertainGantt.Task (TaskName)

newtype Resource = Resource String
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

unResource :: Resource -> String
unResource (Resource r) = r

data DurationD
  = UniformD Word Word
  | NormalD Double Double
  | LogNormalD Double Double
  deriving stock (Eq, Ord, Show)

data Statement
  = AddTask TaskDescription
  | AddResource ResourceDescription
  | DurationAlias String DurationD
  | PrintExample
  | PrintTasks Bool
  | RunSimulations Word
  | PrintCompletionTimes
  | PrintCompletionTimeQuantile Word Word
  | PrintCompletionTimeMean
  deriving stock (Eq, Ord, Show)

data TaskDescription = TaskDescription TaskName String Resource (Either String DurationD) [TaskName]
  deriving stock (Eq, Ord, Show)
data ResourceDescription = ResourceDescription Resource Word
  deriving stock (Eq, Ord, Show)

data MoreInputExpected = ExpectedMultilineInput
  deriving stock (Eq, Ord, Show)

instance P.ShowErrorComponent MoreInputExpected where
  showErrorComponent _ = ""

data StatementRunner s m = StatementRunner
  { initialState :: m s
  , runAddResource :: ResourceDescription -> s -> m s
  , runAddTask :: TaskDescription -> s -> m s
  , runDurationAlias :: (String, DurationD) -> s -> m s
  , runPrintExample :: s -> m s
  , runPrintTasks :: Bool -> s -> m s
  , runSimulations :: Word -> s -> m s
  , runPrintCompletionTimes :: s -> m s
  , runPrintCompletionTimeMean :: s -> m s
  , runPrintCompletionTimeQuantile :: (Word, Word) -> s -> m s
  }
