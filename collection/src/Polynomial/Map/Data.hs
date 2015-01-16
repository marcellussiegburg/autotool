{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language DeriveDataTypeable #-}

module Polynomial.Map.Data where

import Polynomial.Class
import Prelude hiding ( Num (..), Integer, sum)

import Polynomial.Base
       
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Lens.At
import Control.Monad ( guard, mzero )
import Control.Applicative ((<$>))
import Data.Typeable
import Data.Function ( on )

import Autolib.TES.Identifier -- for specializations

-- * polynomials

-- | for the moment, this is monomorphic.
-- in theory, coefficient domain should be some ring.
-- invariant: store only monomials with coefficient /= 0
newtype Poly r v = Poly ( M.Map (Mono v) r ) 
    deriving ( Eq, Ord, Typeable )

valid (Poly m) = all (> 0) $ M.elems m
    
constant :: (Ord v, Ring r) => r -> Poly r v
constant c = monomial (mono []) c

monomial m c = Poly $ M.fromList [ (m, c) | c /= zero ]
         
variable v = monomial (mono [factor v 1]) one

absolute (Poly m) = M.findWithDefault zero (mono []) m         

splitLeading (Poly m) = maybe mzero return $ do
   ((m,c), rest) <- M.maxViewWithKey m
   return ((c,m), Poly rest)
         
null (Poly m) = M.null m

terms (Poly m) = Prelude.map ( \(m,c) -> (c,m) ) $ M.toList m
nterms (Poly m) = M.size m

map f (Poly m) = Poly $ M.map f m


