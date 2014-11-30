module Polynomial.Type 

( Poly(), poly, nterms, terms, variable, absolute
, Mono (), mono, factors
, Factor (), var, expo
, egcd, divMod, gcd
, Identifier
)

where

import Polynomial.Data
import Polynomial.ToDoc
import Polynomial.Reader
import Polynomial.Op
import Autolib.TES.Identifier
import Prelude hiding ( null, divMod, gcd )
