{-# language StandaloneDeriving #-}

module Polynomial.Data where

import Autolib.ToDoc
import Autolib.Reader

import qualified Data.Map.Strict as M

-- | for the moment, this is monomorphic.
-- in theory, coefficient domain should be some ring.
-- invariant: store only monomials with coefficient /= 0
data Poly v = Poly { unPoly :: M.Map (Mono v) Integer }
    deriving Show

poly :: Ord v => [(Integer, Mono v)] -> Poly v
poly cms = Poly 
    { unPoly = M.fromList $ do
        (c,m) <- cms 
        guard $ c /= 0
        return (m,c)
    }

terms p = do (m,c) <- M.toList $ unPoly p ; return (c,m)

-- | invariant: store only factors with nonzero exponents.
-- implementation note: total_degree comes first,
-- and this is used in the derived Ord instance
-- (which then gives length-lexicographic)
data Mono v = Mono { total_degree :: Integer
                   , unMono :: M.Map v Integer 
                   }
    deriving Show 

deriving instance Eq v => Eq (Mono v)
deriving instance Ord v => Ord (Mono v)

mono :: Ord v => [Factor v] -> Mono v
mono fs = Mono 
    { unMono = M.fromList $ do 
        f <- fs 
        guard $ expo f /= 0 
        return (var f, expo f)
    , total_degree = sum $ map expo fs
    }

factors :: Mono v -> [Factor v]
factors m = do 
    (v,e) <- M.toList $ unMono m
    return $ Factor { var = v, expo = e }

data Factor v = Factor { var :: v, expo :: Integer } 
    deriving Show

deriving instance Eq v => Eq (Factor v)
deriving instance Ord v => Ord (Factor v)

