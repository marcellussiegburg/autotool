{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language DeriveDataTypeable #-}

module Polynomial.Pattern where

import Autolib.ToDoc
import Autolib.Reader

import Polynomial.Class
import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, null, gcd, divMod, div, mod )

import Control.Applicative ((<$>),(<*>))
import Data.Typeable

class Pattern p where
     type Base p 
     match :: p -> Base p -> Bool
     base :: p -> Maybe (Base p)

data Patch v = Any | This v deriving Typeable

instance ToDoc v => ToDoc (Patch v) where
    toDoc p = case p of
        Any -> text "_" ; This v -> toDoc v
instance Reader v => Reader (Patch v) where
    reader = do my_reservedOp "_" ; return Any
        <|> ( This <$> reader )

instance Pattern (Patch Integer) where
    type Base (Patch Integer) = Integer
    match p v = case p of
        Any -> True ; This w -> w == v
    base p = case p of
        Any -> Nothing
        This w -> Just w

instance Pattern (Patch (Complex (Patch Integer))) where
    type Base (Patch (Complex (Patch Integer))) = Complex Integer
    match p (a :+ b) = case p of
        Any -> True
        This (p :+ q) -> match p a && match q b
    base p = case p of
        Any -> Nothing
        This (p :+ q) -> (:+) <$> base p <*> base q

instance Pattern (Patch (Ratio (Patch Integer))) where
    type Base (Patch (Ratio (Patch Integer))) = Ratio Integer
    match p (a :% b) = case p of
        Any -> True
        This (p :% q) -> match p a && match q b
    base p = case p of
        Any -> Nothing
        This (p :% q) -> (:%) <$> base p <*> base q



