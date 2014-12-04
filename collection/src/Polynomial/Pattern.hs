{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveDataTypeable #-}

module Polynomial.Pattern where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Util.Zufall

import qualified Polynomial.Unary as U

import Polynomial.Patch

import Polynomial.Class
import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, null, gcd, divMod, div, mod )

import Control.Applicative ((<$>),(<*>))
import Control.Monad (forM)
import Data.Typeable


instance Pattern (Patch Integer) where
    type Base (Patch Integer) = Integer
    inject i = This i
    match p v = case p of
        Any -> True ; This w -> w == v
    base p = case p of
        Any -> 2
        This w -> w
    robfuscate = patch

patch b = eins [ Any, This b ]

instance Pattern (Patch (Complex (Patch Integer))) where
    type Base (Patch (Complex (Patch Integer))) = Complex Integer
    match p (a :+ b) = case p of
        Any -> True
        This (p :+ q) -> match p a && match q b
    inject (a :+ b) = This (This a :+ This b)
    base p = case p of
        Any -> (2 :+ 3)
        This (p :+ q) -> base p :+ base q
    robfuscate (a :+ b) = 
        ( (:+) <$> patch a <*> patch b ) >>= patch
        
instance Pattern (Patch (Ratio (Patch Integer))) where
    type Base (Patch (Ratio (Patch Integer))) = Ratio Integer
    match p (a :% b) = case p of
        Any -> True
        This (p :% q) -> match p a && match q b
    inject (a :% b) = This (This a :% This b)
    base p = case p of
        Any -> 2 % 3
        This (p :% q) -> base p % base q
    robfuscate (a :% b) = 
        ( (:%) <$> patch a <*> patch b ) >>= patch

instance (Pattern a, Pattern b) => Pattern (a, b) where
    type Base (a, b) = (Base a,Base b)
    match (p,q) (a,b) = match p a && match q b
    inject (a,b) = (inject a, inject b)
    base (p,q) = (base p, base q)
    robfuscate (a,b) = (,) <$> robfuscate a <*> robfuscate b

instance Pattern e => Pattern [e]  where
    type Base [e] = [ Base e ]
    match xs ys = length xs == length ys 
        && and ( zipWith match xs ys )
    inject ys = map inject ys
    base xs = map base xs
    robfuscate ys = forM ys robfuscate

type PP p = U.P p (Patch Integer)
instance (Ring (Base p), Pattern p) => Pattern (PP p) where
    type Base (PP p) = U.Poly (Base p)
    match (U.P xs) p = match xs $ U.terms p
    inject p = U.P $ inject $ U.terms p
    base (U.P xs) = U.poly $ base xs
    robfuscate p = U.P <$> robfuscate ( U.terms p )
