{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
    
module Polynomial.Base.Test where

import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, null, gcd, divMod, div, mod )

import Polynomial.Base
import Polynomial.ToDoc ()
import Autolib.ToDoc       
       
import Control.Applicative       
import Polynomial.Class (spec)
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series
import Control.Monad
       
-- * tests
   
mono_spec = describe "monomials" $ do
   describe "order" $ do
      it "stable" $ property $  \ (a::Mono V) b c ->
          (a <= b) <= (multMono a c <= multMono b c)
      it "reflexive" $ property $ \ (m::Mono V) -> m <= m
      it "transitive" $ property $ \ (a::Mono V) b c ->
          ((a <= b) && (b <= c)) <= (a <= c)
      it "antisymmetric" $ property $ \ (m::Mono V) n ->
          (m <= n && m >= n) <= (m == n)
   it "constructor-decreasing" $ property $ \ m ->
       is_decreasing (m :: Mono V )
   it "expos-positive" $ property $ \ m ->
       expos_are_positive (m :: Mono V )
   it "factors-mono" $ property $ \ m ->
       mono (factors m) == (m :: Mono V)
   
data V = X | Y | Z deriving (Eq, Ord, Show)
instance ToDoc V where toDoc = text . show     

instance Monad m => Serial m V where
    series = return X `mplus` return Y `mplus` return Z

instance (Ord v, Serial m v) => Serial m (Mono v) where
    series = mono <$> series
    
instance  Serial m v => Serial m (Factor v) where
    series = Factor
           <$> series
           <*> fmap (\ (Positive p) -> p) series
    
instance ToDoc v => Show (Mono v) where show = render . toDoc 
