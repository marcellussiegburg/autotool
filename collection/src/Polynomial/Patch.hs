{-# language DeriveDataTypeable #-}
{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Polynomial.Patch where

import Data.Typeable
import Autolib.Reader
import Autolib.ToDoc
import Control.Applicative hiding ((<|>))

class Pattern p where

     type Base p 

     match :: p -> Base p -> Bool

     pmap :: (Base p -> Base p) -> (p -> p)

     inject :: Base p -> p

     -- | create and element of the base type.
     -- that matches the pattern
     base :: p -> Base p

     -- | randomly obfuscate
     robfuscate :: Base p -> IO p

     -- | create any element of the base type
     -- note: with dummy argument (not evaluatued)
     -- for type inference
     default_ :: p -> Base p

     -- any attempt to call this function
     -- will give "type function not injective"
     -- default_ :: Base p
     

data Patch v = Any | This v deriving (Typeable, Eq)

instance Functor Patch where
    fmap f p = case p of Any -> Any ; This x -> This $ f x

instance ToDoc v => ToDoc (Patch v) where
    toDoc p = case p of
        Any -> text "_" ; This v -> toDoc v
instance Reader v => Reader (Patch v) where
    reader = do my_reservedOp "_" ; return Any
        <|> ( This <$> reader )

newtype Id a = Id {unId :: a}
    deriving  (Reader, ToDoc)

instance Eq a => Pattern (Id a) where
    type Base (Id a) = a
    inject = Id
    match (Id p) v = p == v
    -- pmap = id
    base (Id p) = p
    default_ _ = undefined
    robfuscate x = return $ Id x


