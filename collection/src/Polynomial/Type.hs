module Polynomial.Type 

( Poly(), poly, nterms, terms, variable, absolute
, Mono (), mono, factors
, Factor (), var, expo
, egcd, divMod, gcd
, Identifier
)

where

import Polynomial.Class

import Polynomial.Data

import Polynomial.ToDoc
import Polynomial.Reader

import Autolib.TES.Identifier
import qualified Prelude

