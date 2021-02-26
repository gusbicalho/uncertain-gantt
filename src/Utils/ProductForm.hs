{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Utils.ProductForm (
  ToProductForm,
  toProductForm,
) where

import Data.Kind (Constraint, Type)
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage (..), Nat, Symbol, TypeError, type (+))

type ToProductForm constructorName constructorFields productForm =
  ( TooManyFieldsInConstructor constructorName constructorFields
  , ToProductForm' constructorFields productForm
  )

type NoFields = G.U1
type Field meta a = G.S1 meta (G.Rec0 a)
type TwoFields metaA a metaB b = Field metaA a G.:*: Field metaB b
type ThreeFields metaA a metaB b metaC c = Field metaA a G.:*: Field metaB b G.:*: Field metaC c

class ToProductForm' rep tuple | rep -> tuple where
  toProductForm :: rep a -> tuple

instance ToProductForm' NoFields () where
  toProductForm _ = ()
  {-# INLINE toProductForm #-}

instance ToProductForm' (Field _meta a) a where
  toProductForm (G.M1 (G.K1 a)) = a
  {-# INLINE toProductForm #-}

instance ToProductForm' (TwoFields _metaA a _metaB b) (a, b) where
  toProductForm ((toProductForm -> a) G.:*: (toProductForm -> b)) = (a, b)
  {-# INLINE toProductForm #-}

instance ToProductForm' (ThreeFields _metaA a _metaB b _metaC c) (a, b, c) where
  toProductForm ((toProductForm -> a) G.:*: (toProductForm -> (b, c))) = (a, b, c)
  {-# INLINE toProductForm #-}

type family TooManyFieldsInConstructor (name :: Symbol) (c :: k -> Type) where
  TooManyFieldsInConstructor name constructor =
    TooManyFieldsInConstructor' name (CountFieldsInConstructor constructor)

type family CountFieldsInConstructor (c :: k -> Type) where
  CountFieldsInConstructor (left G.:*: right) = CountFieldsInConstructor left + CountFieldsInConstructor right
  CountFieldsInConstructor _ = 1

type family TooManyFieldsInConstructor' (name :: Symbol) (n :: Nat) :: Constraint where
  TooManyFieldsInConstructor' name 0 = ()
  TooManyFieldsInConstructor' name 1 = ()
  TooManyFieldsInConstructor' name 2 = ()
  TooManyFieldsInConstructor' name 3 = ()
  TooManyFieldsInConstructor' name fieldCount =
    TypeError
      ( 'Text "Constructor " ':<>: 'Text name ':<>: 'Text " requires "
          ':<>: 'ShowType fieldCount
          ':<>: 'Text " fields."
            ':$$: 'Text "Only constructors with at most 3 fields are supported."
      )
