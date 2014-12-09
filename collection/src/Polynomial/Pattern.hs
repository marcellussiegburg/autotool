{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language FlexibleContexts #-}
{-# language DeriveDataTypeable #-}
{-# language ScopedTypeVariables #-}

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

instance Pattern Integer where
    type Base Integer = Integer
    inject i = i
    match p v = p == v
    pmap = id
    base p = p
    default_ _ = 1
    robfuscate = return

instance Pattern p => Pattern (Patch p) where
    type Base (Patch p) = Base p
    inject i = This $ inject i
    pmap f = fmap (pmap f)
    match p v = case p of
        Any -> True ; This w -> match w v
    base (p :: Patch p) = case p of
        Any -> default_ (undefined :: p)
        This w -> base w
    robfuscate b = do
        action <- eins [ return Any, This <$> robfuscate b ]
        action

instance Pattern p => Pattern (Complex p) where
    type Base (Complex p) = Complex (Base p)
    match (p :+ q) (a :+ b) = match p a && match q b
    inject (a :+ b) = inject a :+ inject b
    -- pmap f (a :+ b) = pmap f a :+ pmap f b
    base (a :+ b) = base a :+ base b
    default_ (_ :: Complex p) = 
       default_ (undefined :: p) :+ default_ (undefined :: p)
    robfuscate (a :+ b) = 
        ( (:+) <$> robfuscate a <*> robfuscate b ) 

instance Pattern p => Pattern (Ratio p) where
    type Base (Ratio p) = Ratio (Base p)
    match (p :% q) (a :% b) = match p a && match q b
    inject (a :% b) = inject a :% inject b
    -- pmap f (a :% b) = pmap f a :% pmap f b
    base (a :% b) = base a :% base b
    default_ (_ :: Ratio p) = 
       default_ (undefined :: p) :% default_ (undefined :: p)
    robfuscate (a :% b) = 
        ( (:%) <$> robfuscate a <*> robfuscate b ) 

instance (Pattern a, Pattern b) => Pattern (a, b) where
    type Base (a, b) = (Base a,Base b)
    match (p,q) (a,b) = match p a && match q b
    inject (a,b) = (inject a, inject b)
    -- pmap f (a,b) = ( pmap f a , pmap f b )
    base (p,q) = (base p, base q)
    default_ (_ :: (a, b)) = 
      (default_ (undefined :: a) , default_ (undefined :: b))
    robfuscate (a,b) = (,) <$> robfuscate a <*> robfuscate b

instance Pattern e => Pattern [e]  where
    type Base [e] = [ Base e ]
    match xs ys = length xs == length ys 
        && and ( zipWith match xs ys )
    inject ys = map inject ys
    -- pmap f = fmap (pmap f)
    base xs = map base xs
    robfuscate ys = forM ys robfuscate

instance (Pattern c, Ring (Base c)
         , Pattern e, Base e ~ Integer
         ) => Pattern (U.P c e) where
    type Base (U.P c e) = U.Poly (Base c)
    match (U.P xs) p = match xs $ U.terms p
    inject p = U.P $ inject $ U.terms p
    base (U.P xs) = U.poly $ base xs
    robfuscate p = U.P <$> robfuscate ( U.terms p )
