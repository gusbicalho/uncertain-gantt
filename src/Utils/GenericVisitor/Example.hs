{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utils.GenericVisitor.Example where

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
  -- MyVisitor can visit both Foo and (Bar String) values, because all we care about
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

{-
Large constructors (i.e. with 4+ fields) are not supported
>>> data DataWithLargeConstructor = LargeConstructor Int Int Int Int deriving stock G.Generic
>>> GV.visit MyVisitor (LargeConstructor 1 2 3 4)
Constructor LargeConstructor requires 4 fields.
Only constructors with at most 3 fields are supported.


MyVisitor cannot visit a type if it does not handle all its constructors names.
>>> data Quux = A | Quux Int deriving stock G.Generic
>>> GV.visit (MyVisitor "Hi") A
No instance for (VisitNamed "Quux" Int MyVisitor)
  arising from a use of ‘visit’


MyVisitor cannot visit a type if a constructor has fields of unexpected type.
>>> data Bla = B String deriving stock G.Generic
>>> GV.visit (MyVisitor "Hi") (B "asd")
No instance for (VisitNamed "B" [Char] MyVisitor)
  arising from a use of ‘visit’


MyVisitor cannot visit a type if it has no Generic instance.
>>> data Qwe = Qwe
>>> GV.visit (MyVisitor "Hi") Qwe
No instance for (Generic Qwe) arising from a use of ‘visit’
-}
