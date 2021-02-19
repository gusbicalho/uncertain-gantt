{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
{-# LANGUAGE UndecidableSuperClasses #-}

module Utils.Runner (
  Run (..),
  Named (..),
  RunVariant,
  runVariant,
  UniformRow
) where

import Data.Row.Internal (Extend, KnownSymbol, LT ((:->)), Label (Label), Row (R))
import Data.Row.Variants (Var)
import Data.Row.Variants qualified as Variants
import GHC.TypeLits (Symbol)

class
  Monad m =>
  Run runner m message result
    | runner -> m
    , runner message -> result
  where
  run :: runner -> message -> m result

newtype Named (name :: Symbol) a = Named a

class
  RunVariant runner m messageRow resultRow
    | runner messageRow -> resultRow
  where
  runVariant :: runner -> Var messageRow -> m (Var resultRow)

instance RunVariant runner m Variants.Empty Variants.Empty where
  runVariant _ = Variants.impossible

instance
  ( KnownSymbol label
  , Variants.WellBehaved ( 'R ((label ':-> message) ': pairs))
  , Variants.WellBehaved ( 'R ((label ':-> result) ': moreResults))
  , Extend label result ( 'R moreResults) ~ 'R ((label ':-> result) ': moreResults)
  , Run runner m (Named label message) result
  , RunVariant runner m ( 'R pairs) ( 'R moreResults)
  ) =>
  RunVariant runner m ( 'R ((label ':-> message) ': pairs)) ( 'R ((label ':-> result) ': moreResults))
  where
  runVariant runner var = case Variants.trial var label of
    Right message -> do
      result <- run runner (Named @label message)
      pure $ Variants.IsJust label result
    Left var' -> do
      result <- runVariant runner var'
      pure $ Variants.extend @result label result
   where
    label = Label @label

type UniformRow :: k -> Row k -> Row k
type family UniformRow value row where
  UniformRow value ( 'R pairs) = 'R (UniformRowR value pairs)

type UniformRowR :: k -> [LT k] -> [LT k]
type family UniformRowR value pairs where
  UniformRowR value '[] = '[]
  UniformRowR value ((label ':-> _) : morePairs) =
    (label ':-> value) ': UniformRowR value morePairs
