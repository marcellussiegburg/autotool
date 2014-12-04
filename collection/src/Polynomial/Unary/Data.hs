{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Polynomial.Unary.Data where

import Polynomial.Class
import Prelude hiding ( Num (..), (/), div, Integer, sum)
import qualified Prelude

import qualified Polynomial.Type as P

import Autolib.ToDoc
import Autolib.Reader

import qualified Data.Map.Strict as M

import Test.SmallCheck.Series
import Test.SmallCheck
import Control.Applicative

data P c e = P [(c, e)]

instance (Serial m c) => Serial m (P c Integer) where
    series = fmap ( \ (P ces) -> P $ map ( \(c,e) ->(c, Prelude.abs e)) ces ) $ newtypeCons P

data Poly c = Poly { coeff :: M.Map Integer c }
    deriving Eq

instance (Ring c, Serial m c) => Serial m (Poly c) where
    series =  ( \ (P ces) -> poly ces ) <$> series

degree p = case M.maxViewWithKey $ coeff p of
    Just ((k,v),_) -> Just k
    Nothing -> Nothing

lmRed p = case M.maxViewWithKey $ coeff p of
    Nothing -> Nothing
    Just ((e,c),q) -> Just ((c,e),Poly {coeff=q}        )

poly :: Ring c => [(c,Integer)] -> Poly c
poly ces = Poly { coeff = M.filter (/= zero) 
                        $ M.fromListWith (+)
                        $ map (\(c,e) -> (e,c))
                        $ ces
                }

terms :: Poly c -> [(c,Integer)]
terms p = map (\(e,c)->(c,e)) $ M.toList $ coeff p

constant c = poly [(c, 0)]

    
