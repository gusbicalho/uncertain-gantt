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
import GHC.Generics (Generic)
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
  deriving stock (Eq, Ord, Show, Generic)

data PrintGanttType = Random | Average
  deriving stock (Eq, Ord, Show, Generic)

data Statement
  = AddTask TaskDescription
  | AddResource ResourceDescription
  | DurationAliasDeclaration String DurationD
  | PrintDuration (Either String DurationD)
  | PrintGantt PrintGanttType
  | PrintTasks Bool
  | RunSimulations Word
  | PrintCompletionTimes
  | PrintCompletionTimeQuantile Word Word
  | PrintCompletionTimeMean
  | PrintHistogram Word
  deriving stock (Eq, Ord, Show, Generic)

data TaskDescription = TaskDescription TaskName String Resource (Either String DurationD) [TaskName]
  deriving stock (Eq, Ord, Show)
data ResourceDescription = ResourceDescription Resource Word
  deriving stock (Eq, Ord, Show)

data MoreInputExpected = ExpectedMultilineInput
  deriving stock (Eq, Ord, Show)

instance P.ShowErrorComponent MoreInputExpected where
  showErrorComponent _ = ""
