module Polynomial.Op where

import Polynomial.Data

import Polynomial.Class
import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, null, gcd, divMod, div, mod )

import Control.Lens
import Control.Applicative
import qualified Data.Map.Strict as M


instance (Ring r, Ord v) => Ring (Poly r v) where
    fromInteger i = constant $ fromInteger i
    zero = fromInteger 0 ; one = fromInteger 1
    negate p = Poly { _unPoly = M.map negate $ p ^. unPoly }
    p + q = Poly { _unPoly = M.filter (/= zero) $ M.unionWith (+) (p ^. unPoly) (q ^.unPoly) }
    p * q = poly $ do
        (c , f) <- terms p ; (d , g) <- terms q 
        return (c*d, monoMult f g)

monoMult p q = Mono 
                 { _unMono = M.filter (/= 0) $ M.unionWith (+) (p ^. unMono) (q ^. unMono) 
                 , _total_degree = p ^. total_degree + q ^. total_degree
                 }

-- | somewhat risky (we don't check the variable)
instance (Field r, Ord v ) => Euclidean_Ring (Poly r v) where
    norm p = ( ^. total_degree ) <$> lt p 
    div a b = case lmRed b of
      Nothing -> error "Polynomial.op.divMod: divide by 0"
      Just ((lcb,ltb),redb) -> case lmRed a of
        Just ((lca, lta),reda) 
          | lta ^. total_degree >= ltb ^. total_degree 
          ->
            let t = poly [(lca / lcb, divMono lta ltb)]
                q  = div (reda - t * redb) b
            in  t + q
        _ -> zero

