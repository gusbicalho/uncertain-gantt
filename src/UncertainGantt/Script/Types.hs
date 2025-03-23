{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module UncertainGantt.Script.Types where

import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Symbolize (Symbol)
import Text.Megaparsec qualified as P
import UncertainGantt.Script.ToText (ToText)
import UncertainGantt.Task (TaskName)

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

data TaskDescription = TaskDescription TaskName Text Resource (Either DurationAlias DurationD) [TaskName]
  deriving stock (Eq, Ord, Show)
data ResourceDescription = ResourceDescription Resource Word
  deriving stock (Eq, Ord, Show)

data MoreInputExpected = ExpectedMultilineInput
  deriving stock (Eq, Ord, Show)

instance P.ShowErrorComponent MoreInputExpected where
  showErrorComponent _ = ""
