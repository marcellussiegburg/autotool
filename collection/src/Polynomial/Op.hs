module Polynomial.Op where

import Polynomial.Data

import Control.Lens
import qualified Data.Map.Strict as M

-- actually this should be Ring, not Num
instance Ord v => Num (Poly v) where
    fromInteger i = poly [ (i, mono [] ) ]
    negate p = Poly { _unPoly = M.map negate $ p ^. unPoly }
    p + q = Poly { _unPoly = M.filter (/= 0) $ M.unionWith (+) (p ^. unPoly) (q ^.unPoly) }
    p * q = poly $ do
        (c , f) <- terms p ; (d , g) <- terms q 
        return (c*d, f*g)

instance Ord v => Num (Mono v) where
    p * q = Mono { _unMono = M.unionWith (+) (p ^. unMono) (q ^. unMono) 
                 , _total_degree = p ^. total_degree + q ^. total_degree
                 }
