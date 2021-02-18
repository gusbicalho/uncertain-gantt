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
  RecordRunner (..),
  RunNamed (runNamed),
  Named (..),
  RunVariant,
  runVariant,
  UniformRow
) where

import Data.Kind (Type)
import Data.Row.Internal (Extend, KnownSymbol, LT ((:->)), Label (Label), Row (R))
import Data.Row.Variants (Var)
import Data.Row.Variants qualified as Variants
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)

class
  Monad m =>
  Run runner m message result
    | runner -> m
    , runner message -> result
  where
  run :: runner -> message -> m result

class
  Monad m =>
  RunNamed (name :: Symbol) runner m message result
    | runner -> m
    , runner name message -> result
  where
  runNamed :: runner -> message -> m result

newtype RecordRunner (m :: Type -> Type) record = RecordRunner record

newtype Named (label :: Symbol) t = Named t

instance
  ( Monad m
  , HasField fieldName runner (message -> m result)
  ) =>
  RunNamed fieldName (RecordRunner m runner) m message result
  where
  runNamed (RecordRunner runner) message = getField @fieldName runner message

instance
  RunNamed name runner m message result =>
  Run runner m (Named name message) result
  where
  run runner (Named message) = runNamed @name runner message

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
  , RunNamed label runner m message result
  , RunVariant runner m ( 'R pairs) ( 'R moreResults)
  ) =>
  RunVariant runner m ( 'R ((label ':-> message) ': pairs)) ( 'R ((label ':-> result) ': moreResults))
  where
  runVariant runner var = case Variants.trial var label of
    Right message -> do
      result <- runNamed @label runner message
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
