module Polynomial.Unary where

import Polynomial.Class
import Prelude hiding ( Num (..), (/), div, Integer, sum)

import qualified Polynomial.Type as P

import Autolib.ToDoc
import Autolib.Reader

import qualified Data.Map.Strict as M

data P c e = P [(c, e)]


data Poly c = Poly { coeff :: M.Map Integer c }
    deriving Eq

degree p = case M.maxViewWithKey $ coeff p of
    Just ((k,v),_) -> Just k
    Nothing -> Nothing

lmRed p = case M.maxViewWithKey $ coeff p of
    Nothing -> Nothing
    Just ((e,c),q) -> Just ((c,e),Poly {coeff=q}        )

poly :: Ring c => [(c,Integer)] -> Poly c
poly ces = Poly { coeff = M.filter (/= zero) 
                        $ M.fromListWith (+)
                        $ map (\(c,e) -> (e,c))
                        $ ces
                }

terms :: Poly c -> [(c,Integer)]
terms p = map (\(e,c)->(c,e)) $ M.toList $ coeff p

constant c = poly [(c, 0)]

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


    
