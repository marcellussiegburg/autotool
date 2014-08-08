{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}

module Rewriting.Termination.Domains where

import Autolib.Reader
import Autolib.ToDoc

import Control.Monad ( when )
import Control.Applicative ( (<$>) )
import Data.Typeable

data Fuzzy = FuzzyMinf | FuzzyFinite Integer | FuzzyPinf
    deriving (Eq, Ord, Typeable)
instance ToDoc Fuzzy where 
    toDoc a = case a of 
        FuzzyMinf -> text "-inf" 
        FuzzyFinite a -> toDoc a
        FuzzyPinf -> text "+inf" 
instance Reader Fuzzy where
    reader = ( const FuzzyPinf <$> my_reserved "+inf" )
         <|> ( const FuzzyMinf <$> my_reserved "-inf" )
         <|> ( FuzzyFinite <$> reader )

data Tropical = TropicalFinite Integer | TropicalInf
    deriving (Eq, Ord, Typeable)
instance ToDoc Tropical where 
    toDoc a = case a of 
        TropicalFinite a -> toDoc a
        TropicalInf -> text "+inf" 
instance Reader Tropical where
    reader = ( const TropicalInf <$> my_reserved "+inf" )
         <|> ( TropicalFinite <$> reader )

data Arctic = ArcticInf | ArcticFinite Integer 
    deriving (Eq, Ord, Typeable)
instance ToDoc Arctic where 
    toDoc a = case a of 
        ArcticInf -> text "-inf" 
        ArcticFinite a -> toDoc a
instance Reader Arctic where
    reader = ( const ArcticInf <$> my_reserved "-inf" )
         <|> ( ArcticFinite <$> reader )

newtype Natural = Natural Integer  deriving ( Eq, Ord, Typeable )
instance ToDoc Natural where toDoc (Natural i) = toDoc i
instance Reader Natural where
    reader = do 
        i <- reader
        when (i < 0) $ fail "number cannot be negative"
        return $ Natural i


