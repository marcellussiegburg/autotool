module Polynomial.Data

( Poly , terms, nterms, map
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


-- pick one implementation.

       
-- WARNING:
-- the ordering on terms is defined in Polynomial.Base.
-- the implementation of  splitLeading  (in ...Op)
-- must be compatible with that definition,
-- else all the Grobner stuff will break.

-- an implementation that works automagically
-- (because it keeps lists sorted w.r.t. that order)   
import Polynomial.List.Op

-- this should also work (keys in the map are sorted)
-- import Polynomial.Map.Op

-- but this requires extra work, so: DONTUSE
-- import Polynomial.Tree.Data




