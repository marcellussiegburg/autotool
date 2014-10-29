{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}

module Polynomial.Data where

import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad ( guard )

-- * factors (for lack of a better name) like "x^5"

data Factor v = Factor { _var :: v, _expo :: Integer } 
    deriving Show

$(makeLenses ''Factor)

deriving instance Eq v => Eq (Factor v)
deriving instance Ord v => Ord (Factor v)

-- * monomials like "x^5 z^2"

-- | invariant: store only factors with nonzero exponents.
-- implementation note: total_degree comes first,
-- and this is used in the derived Ord instance
-- (which then gives length-lexicographic)
data Mono v = Mono { _total_degree :: Integer
                   , _unMono :: M.Map v Integer 
                   }
    deriving Show 

$(makeLenses ''Mono)

nullMono m = M.null $ m ^. unMono

deriving instance Eq v => Eq (Mono v)
deriving instance Ord v => Ord (Mono v)

mono :: Ord v => [Factor v] -> Mono v
mono fs = Mono 
    { _unMono = M.filter (/= 0) $ M.fromListWith (+) $ do 
        f <- fs 
        return (f ^. var , f ^. expo )
    , _total_degree = sum $ map ( ^. expo ) fs
    }

factors :: Mono v -> [Factor v]
factors m = do 
    (v,e) <- M.toList $ m ^. unMono
    return $ Factor { _var = v, _expo = e }


-- * polynomials

-- | for the moment, this is monomorphic.
-- in theory, coefficient domain should be some ring.
-- invariant: store only monomials with coefficient /= 0
data Poly v = Poly { _unPoly :: M.Map (Mono v) Integer }
    deriving Show

$(makeLenses ''Poly)

poly :: Ord v => [(Integer, Mono v)] -> Poly v
poly cms = Poly 
    { _unPoly = M.filter (/= 0) $ M.fromListWith (+) $ do
        (c,m) <- cms 
        return (m,c)
    }

terms p = do (m,c) <- M.toList $ p ^. unPoly ; return (c,m)



