{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Compose (
  Compose,
  (:.),
  (:$),
) where

import Data.Functor.Compose (Compose)

type f :. g = Compose f g

type family c :$ a where
  (Compose f g) :$ a = f :$ (g :$ a)
  f :$ a = f a
