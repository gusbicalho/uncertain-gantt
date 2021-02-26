{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Utils.GenericVisitor (
  GenericVisitor (..),
  VisitNamed (..),
  QualifiedName,
  CanVisit,
  visit,
) where

import Data.Kind (Constraint, Type)
import GHC.Generics qualified as G
import GHC.TypeLits (Symbol)
import Utils.ProductForm (ToProductForm, toProductForm)

type CanVisit visitor sum =
  ( G.Generic sum
  , VisitGenericSumAsNamedProducts visitor () (G.Rep sum)
  )
visit :: CanVisit visitor sum => visitor -> sum -> VisitorResult visitor
visit t s = visitGenericSum @_ @_ @_ @() t (G.from s)

class GenericVisitor visitor where
  type VisitorResult visitor :: Type

type QualifiedName = (Symbol, Symbol, Symbol, Symbol)

class GenericVisitor visitor => VisitNamed (name :: QualifiedName) entry visitor where
  visitNamed :: visitor -> entry -> VisitorResult visitor

type VisitGenericSumAsNamedProducts :: forall labelsK k. Type -> labelsK -> (k -> Type) -> Constraint
class
  GenericVisitor visitor =>
  VisitGenericSumAsNamedProducts visitor labels (genericSum :: k -> Type)
  where
  visitGenericSum :: visitor -> genericSum a -> VisitorResult visitor

type DataType meta constructors = G.D1 meta constructors
type SumOf left right = left G.:+: right
type Constructor f s name fields = G.C1 ( 'G.MetaCons name f s) fields

instance
  ( GenericVisitor visitor
  , VisitGenericSumAsNamedProducts visitor '(packageName, moduleName, dataName) constructors
  ) =>
  VisitGenericSumAsNamedProducts
    visitor
    ()
    (DataType ( 'G.MetaData dataName moduleName packageName _nt) constructors)
  where
  visitGenericSum t (G.M1 a) =
    visitGenericSum @_ @_ @_ @'(packageName, moduleName, dataName) t a

instance
  ( GenericVisitor visitor
  , VisitGenericSumAsNamedProducts visitor labels left
  , VisitGenericSumAsNamedProducts visitor labels right
  ) =>
  VisitGenericSumAsNamedProducts visitor labels (SumOf left right)
  where
  visitGenericSum t (G.L1 l) = visitGenericSum @_ @_ @_ @labels t l
  visitGenericSum t (G.R1 r) = visitGenericSum @_ @_ @_ @labels t r

instance
  ( GenericVisitor visitor
  , ToProductForm name fields productForm
  , VisitNamed '(packageName, moduleName, dataName, name) productForm visitor
  ) =>
  VisitGenericSumAsNamedProducts
    visitor
    '(packageName, moduleName, dataName)
    (Constructor _f _s name fields)
  where
  visitGenericSum t (G.M1 (toProductForm -> v)) =
    visitNamed @'(packageName, moduleName, dataName, name) t v
