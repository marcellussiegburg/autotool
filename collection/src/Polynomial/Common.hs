module Polynomial.Common

( module Polynomial.Data
, module Polynomial.Common 
)       

where

import qualified Prelude  
import Prelude
  hiding ( Num (..), Integer, null, negate, fromInteger)

import Polynomial.Class 

import Polynomial.Base -- common types
import Polynomial.Data -- implementation specific

import Control.Applicative

poly :: (Ring r, Ord v) => [ Term r v ] -> Poly r v
poly cms = foldl (+) zero
    $ Prelude.map ( \ (c,m) -> monomial m c ) cms

splitAbsolute :: (Ord v, Ring r) => Poly r v -> ( r, Poly r v )
splitAbsolute p = ( absolute p, p - constant (absolute p))

-- | leading term
lt :: (Ring r, Ord v) => Poly r v -> Maybe (Mono v)
lt p = snd <$> lm p

-- | leading coefficient
lc :: (Ring r, Ord v) => Poly r v -> Maybe r
lc p = fst <$> lm p

-- | leading monomial
lm :: ( Ring r, Ord v ) => Poly r v -> Maybe (r, Mono v)
lm p = fst <$> splitLeading p

-- | reductum
red p = snd <$> splitLeading p

