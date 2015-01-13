module Polynomial.Data

( Poly , poly, terms, nterms
, constant, variable, absolute, splitAbsolute, null, divF
, Mono, mono , nullMono, factors
, Factor, expo, var, factor
, Expo
)
       
where

import qualified Prelude  

-- import Polynomial.Map.Data
-- import Polynomial.Map.Op

import Polynomial.List.Data
import Polynomial.List.Op
