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
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError, type (+))

type CanVisit visitor sum =
  ( VisitNamedSumRep visitor (G.Rep sum)
  , G.Generic sum
  )
visit :: CanVisit visitor sum => visitor -> sum -> VisitorResult visitor
visit t s = visitNamedSumRep t (G.from s)

class GenericVisitor visitor where
  type VisitorResult visitor :: Type

class GenericVisitor visitor => VisitNamed (name :: Symbol) entry visitor where
  visitNamed :: visitor -> entry -> VisitorResult visitor

class GenericVisitor visitor => VisitNamedSumRep visitor (genericSum :: k -> Type) where
  visitNamedSumRep :: visitor -> genericSum a -> VisitorResult visitor

type NullaryCons _f _s name = G.C1 ( 'G.MetaCons name _f _s) G.U1
type ConsField _meta a = G.S1 _meta (G.Rec0 a)
type SingleCons _f _s name fields = G.C1 ( 'G.MetaCons name _f _s) fields

gField :: forall k i1 (c1 :: G.Meta) i2 c2 (p :: k). G.M1 i1 c1 (G.K1 i2 c2) p -> c2
gField (G.M1 (G.K1 a)) = a

instance
  ( GenericVisitor visitor
  , VisitNamedSumRep visitor sum
  ) =>
  VisitNamedSumRep visitor (G.D1 _meta sum)
  where
  visitNamedSumRep t (G.M1 a) = visitNamedSumRep t a

instance
  ( GenericVisitor visitor
  , VisitNamedSumRep visitor cons
  , VisitNamedSumRep visitor moreCons
  ) =>
  VisitNamedSumRep visitor (cons G.:+: moreCons)
  where
  visitNamedSumRep t (G.L1 l) = visitNamedSumRep t l
  visitNamedSumRep t (G.R1 r) = visitNamedSumRep t r

instance
  ( GenericVisitor visitor
  , VisitNamed name () visitor
  ) =>
  VisitNamedSumRep visitor (NullaryCons _f _s name)
  where
  visitNamedSumRep t _ = visitNamed @name t ()

instance
  ( GenericVisitor visitor
  , VisitNamed name a visitor
  ) =>
  VisitNamedSumRep visitor (SingleCons _f _s name (ConsField _meta a))
  where
  visitNamedSumRep t (G.M1 (gField -> a)) = visitNamed @name t a

instance
  {-# OVERLAPPING #-}
  ( GenericVisitor visitor
  , VisitNamed name (a, b) visitor
  ) =>
  VisitNamedSumRep visitor (SingleCons _f _s name (ConsField _metaA a G.:*: ConsField _metaB b))
  where
  visitNamedSumRep t (G.M1 ((gField -> a) G.:*: (gField -> b))) = visitNamed @name t (a, b)

instance
  {-# OVERLAPPING #-}
  ( GenericVisitor visitor
  , VisitNamed name (a, b, c) visitor
  ) =>
  VisitNamedSumRep visitor (SingleCons _f _s name (ConsField _metaA a G.:*: ConsField _metaB b G.:*: ConsField _metaC c))
  where
  visitNamedSumRep t (G.M1 ((gField -> a) G.:*: (gField -> b) G.:*: (gField -> c))) = visitNamed @name t (a, b, c)

instance
  {-# OVERLAPPABLE #-}
  ( GenericVisitor visitor
  , TooManyFieldsInConstructor name (left G.:*: right)
  ) =>
  VisitNamedSumRep visitor (SingleCons _f _s name (left G.:*: right))
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

data Foo
  = A
  | B Int
  | C Bool String
  | D Char Word (Int, Int)
  deriving stock (G.Generic)

{-
-- Example usage:

newtype FooVisitor = FooVisitor String

x :: (String, String, String, String)
x =
  ( visit visitor A
  , visit visitor $ B 42
  , visit visitor $ C True "Wat"
  , visit visitor $ D 'Q' 76 (2, 7)
  )
 where
  visitor = FooVisitor "Here: "

-- >>> x
-- ("Here: A","Here: B 42","Here: C True Wat","Here: D 'Q' 76 (2,7)")

instance GenericVisitor FooVisitor where
  type VisitorResult FooVisitor = String

instance VisitNamed "A" () FooVisitor where
  visitNamed (FooVisitor visitorName) _ =
    visitorName <> "A"

instance VisitNamed "B" Int FooVisitor where
  visitNamed (FooVisitor visitorName) int =
    visitorName <> "B " <> show int

instance VisitNamed "C" (Bool, String) FooVisitor where
  visitNamed (FooVisitor visitorName) (b, s) =
    visitorName <> "C " <> show b <> " " <> s

instance VisitNamed "D" (Char, Word, (Int, Int)) FooVisitor where
  visitNamed (FooVisitor visitorName) (c, w, pair) =
    visitorName <> "D " <> show c <> " " <> show w <> " " <> show pair

-}
