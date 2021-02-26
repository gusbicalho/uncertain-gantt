{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Utils.Agent.Generic (
  RunsActionGenerically,
  GenericAgent (..),
  QualifiedNameToActionName,
  QualifiedNamePart,
  JoinParts,
  PickPart,
  SimpleName,
  (:&>),
  TransformSymbol.Append,
  TransformSymbol.Identity,
  TransformSymbol.Prepend,
  type (TransformSymbol.:>>>),
) where

import Data.Kind (Type)
import GHC.TypeLits (AppendSymbol, Symbol)
import Utils.Agent.Class (Agent (..), NewAgent (..), RunAction (..), RunNamedAction (..))
import Utils.GenericVisitor (CanVisit, GenericVisitor (..), QualifiedName, VisitNamed (..), visit)
import Utils.TransformSymbol (TransformSymbol)
import Utils.TransformSymbol qualified as TransformSymbol

-- | A wrapper that allows an Agent to act as a GenericVisitor
newtype GenericAgent nameTransform agent = GenericAgent {unGenericAgent :: agent}

instance Agent agent => Agent (GenericAgent nameTransform agent) where
  type AgentMonad (GenericAgent nameTransform agent) = AgentMonad agent

instance
  NewAgent agent =>
  NewAgent (GenericAgent nameTransform agent)
  where
  initial = GenericAgent @nameTransform <$> initial

instance
  ( RunsActionGenerically nameTransform action agent
  ) =>
  RunAction action (GenericAgent nameTransform agent)
  where
  run action agent = visit agent action
  {-# INLINE run #-}

{- |
  An Agent for an action is a thing that, if wrapped GenericAgent, implements a
  GenericVisitor for that action, always retuning the new state of the agent.

  This implementation is given by instances defined in this module, which
  delegate work to the custom instances for RunNamedAction defined for the `agent`
  type.
-}
type RunsActionGenerically nameTransform action agent =
  ( Agent agent
  , GenericAgent nameTransform agent `CanVisit` action
  , VisitorResult (GenericAgent nameTransform agent) ~ AgentMonad agent (GenericAgent nameTransform agent)
  )

instance Agent agent => GenericVisitor (GenericAgent nameTransform agent) where
  type VisitorResult (GenericAgent nameTransform agent) = AgentMonad agent (GenericAgent nameTransform agent)

instance
  ( actionLabel ~ QualifiedNameToActionName nameTransform qualifiedName
  , RunNamedAction actionLabel action agent
  ) =>
  VisitNamed
    qualifiedName
    action
    (GenericAgent nameTransform agent)
  where
  visitNamed (GenericAgent r) msg = GenericAgent <$> runNamed @actionLabel msg r
  {-# INLINE visitNamed #-}

type QualifiedNameToActionName :: Type -> QualifiedName -> Symbol
type family QualifiedNameToActionName transform qualifiedName

data extract :&> transform
type instance
  QualifiedNameToActionName (extract :&> transform) qualifiedName =
    TransformSymbol
      transform
      (QualifiedNameToActionName extract qualifiedName)

data JoinParts (separator :: Symbol) (parts :: [QualifiedNamePart])
type PickPart part = JoinParts "" '[part]
type SimpleName = PickPart ConstructorName

data QualifiedNamePart = PackageName | ModuleName | DatatypeName | ConstructorName

type instance
  QualifiedNameToActionName (JoinParts separator parts) qualifiedName =
    JoinParts' separator parts qualifiedName

type JoinParts' :: Symbol -> [QualifiedNamePart] -> QualifiedName -> Symbol
type family JoinParts' separator parts qualifiedName where
  JoinParts' _ '[] _ = ""
  JoinParts' _ '[part] qualifiedName = PickPart' part qualifiedName
  JoinParts' separator (part ': moreParts) qualifiedName =
    PickPart' part qualifiedName
      `AppendSymbol` separator
      `AppendSymbol` JoinParts' separator moreParts qualifiedName

type PickPart' :: QualifiedNamePart -> QualifiedName -> Symbol
type family PickPart' parts qualifiedName where
  PickPart' 'PackageName '(packageName, _, _, _) = packageName
  PickPart' 'ModuleName '(_, moduleName, _, _) = moduleName
  PickPart' 'DatatypeName '(_, _, datatypeName, _) = datatypeName
  PickPart' 'ConstructorName '(_, _, _, constructorName) = constructorName
