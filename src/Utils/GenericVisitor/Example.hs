{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.GenericVisitor.Example where

import qualified Data.List as List
import qualified GHC.Generics as G
import qualified Utils.GenericVisitor as GV

newtype MyVisitor = MyVisitor String

data Foo
  = A
  | B Int
  deriving stock (G.Generic)

data Bar a
  = C Bool a
  | D Char Word (a, a)
  deriving stock (G.Generic)

x :: (String, String, String, String)
x =
  ( GV.visit visitor A
  , GV.visit visitor $ B 42
  , GV.visit visitor $ C True "Wat"
  , GV.visit visitor $ D 'Q' 76 ("abc", "def")
  -- MyVisitor can visit both Foo and Bar values, because all we care about
  -- are constructor names and field types
  )
 where
  visitor = MyVisitor "Here:"

-- >>> x
-- ("Here: A","Here: B 42","Here: C True Wat","Here: D 'Q' 76 abc def")

instance GV.GenericVisitor MyVisitor where
  type VisitorResult MyVisitor = String

instance GV.VisitNamed "A" () MyVisitor where
  visitNamed (MyVisitor visitorName) _ =
    unwords [visitorName, "A"]

instance GV.VisitNamed "B" Int MyVisitor where
  visitNamed (MyVisitor visitorName) int =
    unwords [visitorName, "B", show int]

instance GV.VisitNamed "C" (Bool, String) MyVisitor where
  visitNamed (MyVisitor visitorName) (b, s) =
    unwords [visitorName, "C", show b, s]

instance GV.VisitNamed "D" (Char, Word, (String, String)) MyVisitor where
  visitNamed (MyVisitor visitorName) (c, w, (s1, s2)) =
    unwords [visitorName, "D", show c, show w, s1, s2]
