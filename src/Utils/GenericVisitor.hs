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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Utils.GenericVisitor (
  GenericVisitor (..),
  VisitNamed (..),
  CanVisit,
  visit,
) where

import Data.Kind (Type)
import GHC.Generics qualified as G
import GHC.TypeLits (Symbol)
import Utils.ProductForm (ToProductForm, toProductForm)

type CanVisit visitor sum =
  ( G.Generic sum
  , VisitGenericSumAsNamedProducts visitor (G.Rep sum)
  )
visit :: CanVisit visitor sum => visitor -> sum -> VisitorResult visitor
visit t s = visitGenericSum t (G.from s)

class GenericVisitor visitor where
  type VisitorResult visitor :: Type

class GenericVisitor visitor => VisitNamed (name :: Symbol) entry visitor where
  visitNamed :: visitor -> entry -> VisitorResult visitor

class GenericVisitor visitor => VisitGenericSumAsNamedProducts visitor (genericSum :: k -> Type) where
  visitGenericSum :: visitor -> genericSum a -> VisitorResult visitor

type DataType meta constructors = G.D1 meta constructors
type SumOf left right = left G.:+: right
type Constructor f s name fields = G.C1 ( 'G.MetaCons name f s) fields

instance
  ( GenericVisitor visitor
  , VisitGenericSumAsNamedProducts visitor constructors
  ) =>
  VisitGenericSumAsNamedProducts visitor (DataType _meta constructors)
  where
  visitGenericSum t (G.M1 a) = visitGenericSum t a

instance
  ( GenericVisitor visitor
  , VisitGenericSumAsNamedProducts visitor left
  , VisitGenericSumAsNamedProducts visitor right
  ) =>
  VisitGenericSumAsNamedProducts visitor (SumOf left right)
  where
  visitGenericSum t (G.L1 l) = visitGenericSum t l
  visitGenericSum t (G.R1 r) = visitGenericSum t r

instance
  ( GenericVisitor visitor
  , ToProductForm name fields productForm
  , VisitNamed name productForm visitor
  ) =>
  VisitGenericSumAsNamedProducts visitor (Constructor _f _s name fields)
  where
  visitGenericSum t (G.M1 (toProductForm -> v)) = visitNamed @name t v
