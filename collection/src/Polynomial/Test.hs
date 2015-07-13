{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}

module Polynomial.Test where

import qualified Prelude  
import Prelude hiding ( Num (..), Integer, null, negate, fromInteger)

import Polynomial.Class 
import Polynomial.Common
import Polynomial.ToDoc

import Control.Applicative
import Autolib.ToDoc
import Autolib.Reader
import Autolib.TES.Identifier
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series

test d = spec d
       $ ring_spec (undefined :: Poly Integer Identifier)

leading_spec (_ :: Poly r v) = describe "leading" $ do
  it "valid" $ property $ \ p ->
    case splitLeading (p :: Poly r v) of
      Nothing -> null p
      Just ((c,m), q) -> valid q
  it "sum" $ property $ \ p ->
    case splitLeading (p :: Poly r v) of
      Nothing -> null p
      Just ((c,m), q) -> monomial m c + q == p

valid_spec (_ :: Poly r v) = describe "valid" $ do
  it "poly" $ property $ \ p -> valid (p :: Poly r v)
  it "terms/poly" $ property $ \ p ->
    (p :: Poly r v) == poly (terms p)
  it "fromInteger" $ property $ \ i -> valid (fromInteger i :: Poly r v)
  it "negate" $ property $ \ p -> valid (negate p :: Poly r v)
  it "plus" $ property $ \ p q -> valid (p + q :: Poly r v)
  it "times" $ property $ \ p q -> valid (p * q :: Poly r v)

instance Monad m => Serial m Identifier where
  series = ( \ p -> mk 0 $ "s" ++ show (p::Int) ) <$> series

instance ( Serial m v) => Serial m (Factor v) where
  series = ( \ v (Positive e) -> factor v e ) <$> series <*> series

instance ( Ord v, Serial m v) => Serial m (Mono v) where
  series = mono <$> series

instance (Ord v, Ring r, Serial m r, Serial m v)
         => Serial m (Poly r v) where
  series = poly <$> series

