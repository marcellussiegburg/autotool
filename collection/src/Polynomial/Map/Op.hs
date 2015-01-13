module Polynomial.Map.Op where

import Polynomial.Map.Data

import Polynomial.Class
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
    negate p = Poly { _unPoly = M.map negate $ p ^. unPoly }
    p + q = Poly
      { _unPoly = -- M.filter (/= zero) $ M.unionWith (+) 
           M.mergeWithKey ( \ k a b -> let v = a + b in if v == zero then Nothing else Just v ) id id
           (p ^. unPoly) (q ^.unPoly)
      }
    p * q | nterms p > nterms q = q * p  
    p * q = -- poly $ do (c , f) <- terms p ; (d , g) <- terms q ; return (c*d, monoMult f g)
      foldl (+) zero $ do (c,f) <- terms p ; return $ monoMultPoly (c,f) q
      -- M.foldMapWithKey ( \ f c -> monoMultPoly (c,f) q ) ( p ^. unPoly )




instance Normalize_Fraction (Poly r v) where
    -- RISKY?
    (%) = (:%)

monoMultPoly (c,f) q = Poly
    { _unPoly = M.mapKeysMonotonic ( monoMult f ) $ M.map (c * ) $ q ^. unPoly 
    }

monoMult p q = Mono 
    { _unMono = -- M.filter (/= 0) $ M.unionWith (+)
           -- M.mergeWithKey ( \ k a b -> let v = a + b in if v == zero then Nothing else Just v ) id id
           merge
           (p ^. unMono) (q ^. unMono) 
    , _total_degree = p ^. total_degree + q ^. total_degree
    }

divMono :: Ord v => Mono v -> Mono v -> Mono v
divMono m n =
    let d = merge (m ^. unMono) $ over ( mapped . _2 ) negate (n ^. unMono)
    in  if Prelude.null $ filter ( (< 0) . snd ) d
        then mono $ map (\(k,v) -> Factor k v) d
        else error "Polynomial.data.divMono: exponent<0"

-- | largest comes first
merge [] ys = ys ; merge xs [] = xs
merge (x:xs) (y:ys) = case compare (fst x) (fst y) of
  GT -> x : merge xs (y:ys)
  LT -> y : merge (x:xs) ys
  EQ -> let v = snd x + snd y
        in  if v == zero then merge xs ys
            else (fst x, v) : merge xs ys                  

divF :: Field f => Poly f v -> f -> Poly f v
divF p f = Poly { _unPoly = M.map (\ c -> c / f)
                          $ p ^. unPoly }
