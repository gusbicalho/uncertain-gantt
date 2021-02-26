{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.TransformSymbol (
  TransformSymbol,
  Identity,
  Prepend,
  Append,
  (:>>>),
  Proxy (..),
) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (AppendSymbol, Symbol)
import Data.Kind (Type)

type family TransformSymbol (t :: Type) (s :: Symbol) :: Symbol

data Identity
type instance TransformSymbol Identity name = name

data Prepend (prefix :: Symbol)
type instance TransformSymbol (Prepend prefix) name = prefix `AppendSymbol` name

data Append (suffix :: Symbol)
type instance TransformSymbol (Append suffix) name = name `AppendSymbol` suffix

data transform1 :>>> transform2
type instance TransformSymbol (t1 :>>> t2) name = TransformSymbol t2 (TransformSymbol t1 name)
