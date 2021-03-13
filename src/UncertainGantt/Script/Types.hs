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

{-# LANGUAGE DeriveTraversable #-}
module UncertainGantt.Script.Types where

import Data.String (IsString)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as P
import UncertainGantt.Task (TaskName)
import Data.Void (Void)

newtype Resource = Resource String
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

unResource :: Resource -> String
unResource (Resource r) = r

data DurationAST ref
  = UniformD Word Word
  | NormalD Double Double
  | LogNormalD Double Double
  | ExactD Word
  | DurationAST ref `MinusD` DurationAST ref
  | DurationAST ref `PlusD` DurationAST ref
  | DurationAliasRef !ref
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

type DurationD = DurationAST Void
type DurationExpr = DurationAST String

data PrintGanttType = Random | Average
  deriving stock (Eq, Ord, Show, Generic)

data Statement
  = AddTask TaskDescription
  | AddResource ResourceDescription
  | DurationAliasDeclaration String DurationExpr
  | PrintDuration DurationExpr
  | PrintGantt PrintGanttType
  | PrintTasks Bool
  | RunSimulations Word
  | PrintCompletionTimes
  | PrintCompletionTimeQuantile Word Word
  | PrintCompletionTimeMean
  | PrintHistogram Word
  deriving stock (Eq, Ord, Show, Generic)

data TaskDescription = TaskDescription TaskName String Resource DurationExpr [TaskName]
  deriving stock (Eq, Ord, Show)
data ResourceDescription = ResourceDescription Resource Word
  deriving stock (Eq, Ord, Show)

data MoreInputExpected = ExpectedMultilineInput
  deriving stock (Eq, Ord, Show)

instance P.ShowErrorComponent MoreInputExpected where
  showErrorComponent _ = ""
