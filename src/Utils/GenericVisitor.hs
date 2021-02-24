{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Utils.GenericVisitor (
  Visitor (..),
  Visitor.Visit (..),
  Visitor.VisitNamed (..),
  GenericVisitor (..),
) where

import Data.Kind (Type)
import GHC.Generics qualified as G
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError, type (+))
import Utils.Visitor (Visitor)
import Utils.Visitor qualified as Visitor

-- type CanVisit visitor sum =
--   ( G.Generic sum
--   , VisitNamedSumRep visitor (G.Rep sum)
--   )
-- visit :: CanVisit visitor sum => visitor -> sum -> Visitor.VisitorResult visitor
-- visit t s = visitNamedSumRep t (G.from s)

type GenericVisitor :: Type -> Type -> Type
newtype GenericVisitor result visitor = GenericVisitor visitor

instance Visitor (GenericVisitor result visitor) where
  type VisitorResult (GenericVisitor result visitor) = result

instance
  ( Visitor.VisitNamed label entry visitor
  , result ~ Visitor.VisitorResult visitor
  ) =>
  Visitor.VisitNamed label entry (GenericVisitor result visitor)
  where
  visitNamed (GenericVisitor v) entry = Visitor.visitNamed @label v entry

instance
  ( G.Generic entry
  , VisitNamedSumRep (GenericVisitor result visitor) (G.Rep entry)
  ) =>
  Visitor.Visit entry (GenericVisitor result visitor)
  where
  visit t s = visitNamedSumRep t (G.from s)

class VisitNamedSumRep visitor (genericSum :: k -> Type) where
  visitNamedSumRep :: visitor -> genericSum a -> Visitor.VisitorResult visitor

type NullaryCons _f _s name = G.C1 ( 'G.MetaCons name _f _s) G.U1
type ConsField _meta a = G.S1 _meta (G.Rec0 a)
type SingleCons _f _s name fields = G.C1 ( 'G.MetaCons name _f _s) fields

gField :: forall k i1 (c1 :: G.Meta) i2 c2 (p :: k). G.M1 i1 c1 (G.K1 i2 c2) p -> c2
gField (G.M1 (G.K1 a)) = a

instance
  ( VisitNamedSumRep (GenericVisitor result visitor) sum
  ) =>
  VisitNamedSumRep (GenericVisitor result visitor) (G.D1 _meta sum)
  where
  visitNamedSumRep t (G.M1 a) = visitNamedSumRep t a

instance
  ( VisitNamedSumRep (GenericVisitor result visitor) cons
  , VisitNamedSumRep (GenericVisitor result visitor) moreCons
  ) =>
  VisitNamedSumRep (GenericVisitor result visitor) (cons G.:+: moreCons)
  where
  visitNamedSumRep t (G.L1 l) = visitNamedSumRep t l
  visitNamedSumRep t (G.R1 r) = visitNamedSumRep t r

instance
  ( Visitor.VisitNamed name () (GenericVisitor result visitor)
  ) =>
  VisitNamedSumRep (GenericVisitor result visitor) (NullaryCons _f _s name)
  where
  visitNamedSumRep t _ = Visitor.visitNamed @name t ()

instance
  ( Visitor.VisitNamed name a (GenericVisitor result visitor)
  ) =>
  VisitNamedSumRep (GenericVisitor result visitor) (SingleCons _f _s name (ConsField _meta a))
  where
  visitNamedSumRep t (G.M1 (gField -> a)) = Visitor.visitNamed @name t a

instance
  {-# OVERLAPPING #-}
  ( Visitor.VisitNamed name (a, b) (GenericVisitor result visitor)
  ) =>
  VisitNamedSumRep (GenericVisitor result visitor) (SingleCons _f _s name (ConsField _metaA a G.:*: ConsField _metaB b))
  where
  visitNamedSumRep t (G.M1 ((gField -> a) G.:*: (gField -> b))) = Visitor.visitNamed @name t (a, b)

instance
  {-# OVERLAPPING #-}
  ( Visitor.VisitNamed name (a, b, c) (GenericVisitor result visitor)
  ) =>
  VisitNamedSumRep (GenericVisitor result visitor) (SingleCons _f _s name (ConsField _metaA a G.:*: ConsField _metaB b G.:*: ConsField _metaC c))
  where
  visitNamedSumRep t (G.M1 ((gField -> a) G.:*: (gField -> b) G.:*: (gField -> c))) = Visitor.visitNamed @name t (a, b, c)

instance
  {-# OVERLAPPABLE #-}
  ( TooManyFieldsInConstructor name (left G.:*: right)
  ) =>
  VisitNamedSumRep (GenericVisitor result visitor) (SingleCons _f _s name (left G.:*: right))
  where
  visitNamedSumRep _ _ = error "impossible"

type family CountFieldsInConstructor (c :: k -> Type) where
  CountFieldsInConstructor (left G.:*: right) = CountFieldsInConstructor left + CountFieldsInConstructor right
  CountFieldsInConstructor _ = 1

type family TooManyFieldsInConstructor (name :: Symbol) (c :: k -> Type) where
  TooManyFieldsInConstructor name constructor =
    TypeError
      ( 'Text "Constructor " ':<>: 'Text name ':<>: 'Text " requires "
          ':<>: 'ShowType (CountFieldsInConstructor constructor)
          ':<>: 'Text " fields."
            ':$$: 'Text "Only constructors with at most 3 fields are supported."
      )
