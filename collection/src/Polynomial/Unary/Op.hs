module Polynomial.Unary.Op where

import Polynomial.Unary.Data
import Polynomial.Unary.ToDoc

import Polynomial.Class
import Prelude hiding ( Num (..), (/), div, divMod, Rational, Integer, sum)
import qualified Prelude

import qualified Polynomial.Type as P

import Autolib.ToDoc


import qualified Data.Map.Strict as M

import Test.SmallCheck.Series
import Test.SmallCheck
import Control.Applicative

instance Ring r => Ring (Poly r) where
    zero = constant zero
    one = constant one
    fromInteger = constant . fromInteger
    p + q = Poly { coeff = M.filter (/= zero)
                         $ M.unionWith (+)
                         ( coeff p) (coeff q)
                 }
    negate p = Poly { coeff = M.map negate $ coeff p }
    p * q = poly $ do
        (cp,ep) <- terms p ; (cq,eq) <- terms q
        return (cp * cq, ep + eq)

instance (Field r ) => Euclidean_Ring (Poly r ) where
    norm  = degree
    div a b = case lmRed b of
      Nothing -> error "Polynomial.Unary.div: divide by 0"
      Just ((lcb,ltb),redb) -> case lmRed a of
        Just ((lca, lta),reda) 
          | lta >= ltb
          ->
            let t = poly [(lca / lcb, lta - ltb)]
                q  = div (reda - t * redb) b
            in  t + q
        _ -> zero



