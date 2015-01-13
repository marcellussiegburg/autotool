module Polynomial.Data

( Poly , poly, terms, nterms
, constant, variable, absolute, splitAbsolute, null, divF
, Mono, mono , nullMono, factors
, Factor, expo, var, factor
, Expo
, valid
)
       
where

import qualified Prelude

-- pick one implementation:

import Polynomial.Base
import Polynomial.Tree.Data

-- import Polynomial.Map.Data
-- import Polynomial.Map.Op

-- import Polynomial.List.Data
-- import Polynomial.List.Op

