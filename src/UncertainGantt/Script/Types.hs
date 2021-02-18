{-# LANGUAGE TypeFamilies #-}
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
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module UncertainGantt.Script.Types where

import Data.Kind (Type)
import Data.Row.Dictionaries (Unconstrained1)
import qualified Data.Row.Variants as Variants
import Data.String (IsString)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import qualified Text.Megaparsec as P
import UncertainGantt.Task (TaskName)
import qualified Utils.Runner as Runner

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

data Statement
  = AddTask TaskDescription
  | AddResource ResourceDescription
  | DurationDeclaration (Maybe String, DurationD)
  | PrintExample ()
  | PrintTasks Bool
  | RunSimulations Word
  | PrintCompletionTimes ()
  | PrintCompletionTimeQuantile (Word, Word)
  | PrintCompletionTimeMean ()
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

type RunStmt stmt runner state m =
  ( Functor m
  , Runner.RunNamed "Init" runner m () state
  , Runner.RunVariant
      runner
      m
      (Variants.Map ((,) state) (Variants.NativeRow stmt))
      (Runner.UniformRow state (Variants.NativeRow stmt))
  , Variants.FromNative stmt
  , Variants.Forall
      (Variants.NativeRow stmt)
      Unconstrained1
  , Variants.Forall
      ( Runner.UniformRow
          state
          (Variants.NativeRow stmt)
      )
      ((~) state)
  )

class StatementRunner runner where
  -- WIP make StmtRunner depend on this so we can remove some type params
  -- Also change StmtRunner -> RunStatement
  type RunnerMonad runner :: Type -> Type
  type RunnerState runner :: Type

class
  (Runner.RunNamed label (runner m) m (s, message) s) =>
  StmtRunner (label :: Symbol) message runner s m
  where
  runStmt :: runner m -> message -> s -> m s

instance
  ( Monad m
  , StmtRunner (label :: Symbol) message runner s m
  ) =>
  Runner.RunNamed label (runner m) m (s, message) s
  where
  runNamed runner (s, msg) = runStmt @label runner msg s

initState :: Runner.RunNamed "Init" runner m () state => runner -> m state
initState runner = Runner.runNamed @"Init" runner ()
