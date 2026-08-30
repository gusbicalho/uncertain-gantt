{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- | Statements of the @.ug@ script language. The definition vocabulary the
statements carry lives in "UncertainGantt.Lang.Types"; this module only
adds what is specific to the textual script format.
-}
module UncertainGantt.Script.Types (
  Statement (..),
  PrintGanttType (..),
  MoreInputExpected (..),
) where

import GHC.Generics (Generic)
import Text.Megaparsec qualified as P
import UncertainGantt.Lang.Types (
  DurationAlias,
  DurationD,
  ResourceDescription,
  TaskDescription,
 )

data PrintGanttType = Random | Average
  deriving stock (Eq, Ord, Show, Generic)

data Statement
  = AddTask TaskDescription
  | AddResource ResourceDescription
  | DurationAliasDeclaration DurationAlias DurationD
  | PrintDuration (Either DurationAlias DurationD)
  | PrintGantt PrintGanttType
  | PrintTasks Bool
  | RunSimulations Word
  | PrintCompletionTimes
  | PrintCompletionTimeQuantile Word Word
  | PrintCompletionTimeMean
  | PrintHistogram Word
  deriving stock (Eq, Ord, Show, Generic)

data MoreInputExpected = ExpectedMultilineInput
  deriving stock (Eq, Ord, Show)

instance P.ShowErrorComponent MoreInputExpected where
  showErrorComponent _ = ""
