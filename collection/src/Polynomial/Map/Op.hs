module Polynomial.Map.Op

( module Polynomial.Map.Data
,   module Polynomial.Map.Op
)    

where

import Polynomial.Map.Data

import Polynomial.Class
import Polynomial.Base

import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, null, gcd, divMod, div, mod )

import Control.Lens
import Control.Applicative
import qualified Data.Map.Strict as M
import Data.Monoid

import Autolib.TES.Identifier

instance (Ring r, Ord v) => Monoid (Poly r v) where
    {-# specialize instance Monoid (Poly Integer Identifier) #-}
    mempty = zero ; mappend = (+)
  
instance (Ring r, Ord v) => Ring (Poly r v) where
    {-# specialize instance Ring (Poly Integer Identifier) #-}
    fromInteger i = constant $ fromInteger i
    zero = fromInteger 0 ; one = fromInteger 1
    negate (Poly m) = Poly $ M.map negate m
    Poly p + Poly q = Poly $
      -- M.filter (/= zero) $ M.unionWith (+) 
           M.mergeWithKey ( \ k a b -> let v = a + b in if v == zero then Nothing else Just v ) id id p q
    p * q | nterms p > nterms q = q * p  
    p * q = -- poly $ do (c , f) <- terms p ; (d , g) <- terms q ; return (c*d, monoMult f g)
      foldl (+) zero $ do (c,f) <- terms p ; return $ monoMultPoly (c,f) q
      -- M.foldMapWithKey ( \ f c -> monoMultPoly (c,f) q ) ( p ^. unPoly )

monoMultPoly (c,f) (Poly q) = Poly 
    $ M.mapKeysMonotonic ( multMono f ) $ M.map (c * ) $ q 

divF :: Field f => Poly f v -> f -> Poly f v
divF (Poly p) f = Poly $ M.map (\ c -> c / f) p
