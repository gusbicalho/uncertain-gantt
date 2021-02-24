{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Visitor where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

class Visitor visitor where
  type VisitorResult visitor :: Type

class Visitor visitor => Visit entry visitor where
  visit :: visitor -> entry -> VisitorResult visitor

class Visitor visitor => VisitNamed (name :: Symbol) entry visitor where
  visitNamed :: visitor -> entry -> VisitorResult visitor
