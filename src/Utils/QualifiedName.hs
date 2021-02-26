{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.QualifiedName (
  QualifiedName,
  Symbol,
  QualifiedNameToActionName,
  QualifiedNamePart (..),
  Join,
  Pick,
) where

import Data.Kind (Type)
import GHC.TypeLits (AppendSymbol, Symbol)

type QualifiedName = (Symbol, Symbol, Symbol, Symbol)
data QualifiedNamePart = PackageName | ModuleName | DatatypeName | ConstructorName

type QualifiedNameToActionName :: Type -> QualifiedName -> Symbol
type family QualifiedNameToActionName transform qualifiedName

data Join (separator :: Symbol) (parts :: [QualifiedNamePart])
type Pick part = Join "" '[part]

type instance
  QualifiedNameToActionName (Join separator parts) qualifiedName =
    JoinParts separator parts qualifiedName

type JoinParts :: Symbol -> [QualifiedNamePart] -> QualifiedName -> Symbol
type family JoinParts separator parts qualifiedName where
  JoinParts _ '[] _ = ""
  JoinParts _ '[part] qualifiedName = PickPart part qualifiedName
  JoinParts separator (part ': moreParts) qualifiedName =
    PickPart part qualifiedName
      `AppendSymbol` separator
      `AppendSymbol` JoinParts separator moreParts qualifiedName

type PickPart :: QualifiedNamePart -> QualifiedName -> Symbol
type family PickPart parts qualifiedName where
  PickPart 'PackageName '(packageName, _, _, _) = packageName
  PickPart 'ModuleName '(_, moduleName, _, _) = moduleName
  PickPart 'DatatypeName '(_, _, datatypeName, _) = datatypeName
  PickPart 'ConstructorName '(_, _, _, constructorName) = constructorName
