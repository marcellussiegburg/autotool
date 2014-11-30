module Polynomial.Op where

import Polynomial.Data

import Control.Lens
import qualified Data.Map.Strict as M

import Prelude hiding (divMod, gcd,null)

-- actually this should be Ring, not Num
instance (Num r, Eq r, Ord v) => Num (Poly r v) where
    fromInteger i = constant $ fromInteger i
    negate p = Poly { _unPoly = M.map negate $ p ^. unPoly }
    p + q = Poly { _unPoly = M.filter (/= 0) $ M.unionWith (+) (p ^. unPoly) (q ^.unPoly) }
    p * q = poly $ do
        (c , f) <- terms p ; (d , g) <- terms q 
        return (c*d, f*g)

instance Ord v => Num (Mono v) where
    p * q = Mono { _unMono = M.filter (/= 0) $ M.unionWith (+) (p ^. unMono) (q ^. unMono) 
                 , _total_degree = p ^. total_degree + q ^. total_degree
                 }

-- | somewhat risky (we don't check the variable)
divMod :: (Ord v, Num t, Fractional r, Eq r)
       => Poly r v -> Poly r v -> ( Poly r v, Poly r v )
divMod a b = case lmRed b of
    Nothing -> error "Polynomial.op.divMod: divide by 0"
    Just ((lcb,ltb),redb) -> case lmRed a of
        Just ((lca, lta),reda) 
          | lta ^. total_degree >= ltb ^. total_degree 
          ->
            let t = poly [(lca / lcb, divMono lta ltb)]
                (q,r) = divMod (reda - t * redb) b
            in  (t + q, r)
        _ -> ( 0, a )

gcd a b = let (g,p,q) = egcd a b in (p,q)

egcd a b =
    if null b then (a, 1, 0)
    else let (d, m) = divMod a b
             (g, p', q') = egcd b m
         -- p' * b + q' * m = g
         -- p' * b + q' * (a - d*b)
         -- q'*a + (p'- q'* d) b 
         in  (g, q', p' - q' * d )
