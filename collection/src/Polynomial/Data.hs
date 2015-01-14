module Polynomial.Data

( Poly , terms, nterms
, constant, variable, absolute, null, divF
, Mono, mono, monomial , nullMono, factors
, Factor, expo, var, factor
, Expo
, splitLeading
, valid
)
       
where

import qualified Prelude

import Polynomial.Base


-- pick one implementation:

-- import Polynomial.Tree.Data

-- import Polynomial.Map.Data
-- import Polynomial.Map.Op

import Polynomial.List.Op

