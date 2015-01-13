{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language DeriveDataTypeable #-}

module Polynomial.Map.Data where

import Polynomial.Class
import Prelude hiding ( Num (..), Integer, sum)

import qualified Data.Map.Strict as M
import Control.Lens
import Control.Lens.At
import Control.Monad ( guard )
import Control.Applicative ((<$>))
import Data.Typeable
import Data.Function ( on )

import Autolib.TES.Identifier -- for specializations

-- * factors (for lack of a better name) like "x^5"

type Expo = Int

data Factor v = Factor { _var :: ! v, _expo :: ! Expo } 
    deriving (Typeable, Eq, Ord)

factor v e = Factor { _var = v, _expo = e }

$(makeLenses ''Factor)

-- * monomials like "x^5 z^2"

-- | invariant: store only factors with nonzero exponents.
-- implementation note: total_degree comes first,
-- and this is used in the derived Ord instance
-- (which then gives graded-lexicographic)
data Mono v = Mono { _total_degree :: ! Expo
                   , _unMono :: ! [ (v, Expo ) ]
                   }
    deriving (Typeable, Eq, Ord)

$(makeLenses ''Mono)

nullMono m = Prelude.null $ m ^. unMono

mono :: Ord v => [Factor v] -> Mono v
mono fs = Mono 
    { _unMono = filter ( (/= 0) . snd ) $ do 
        f <- fs 
        return (f ^. var , f ^. expo )
    , _total_degree = sum $ map ( ^. expo ) fs
    }

factors :: Mono v -> [ (v,Expo) ]
factors m =  m ^. unMono

-- * polynomials

-- | for the moment, this is monomorphic.
-- in theory, coefficient domain should be some ring.
-- invariant: store only monomials with coefficient /= 0
data Poly r v = Poly { _unPoly :: ! ( M.Map (Mono v) r ) }
    deriving ( Eq, Typeable )

$(makeLenses ''Poly)

type instance IxValue (Poly r v) = r
type instance Index (Poly r v)  = Mono v
instance Ord v => Ixed (Poly r v) where 
    ix k = unPoly  . ix k 

poly :: (Ring r, Eq r, Ord v) 
     => [(r, Mono v)] -> Poly r v
poly cms = Poly 
    { _unPoly = M.filter (/= zero) $ M.fromListWith (+) $ do
        (c,m) <- cms 
        return (m,c)
    }

terms p = do (m,c) <- M.toList $ p ^. unPoly ; return (c,m)

nterms p = M.size $ p ^. unPoly

variable v = poly [(1, mono [ factor v 1 ])]
constant c = poly [(c, mono[])]

absolute p = M.findWithDefault zero ( mono [] ) $ p ^. unPoly
null = M.null . ( ^. unPoly )

splitAbsolute :: (Ring r, Ord v) 
              => Poly r v -> ( r, Poly r v )
splitAbsolute p = 
    ( absolute p, over unPoly (M.delete (mono [])) p )

-- | leading term
lt :: Poly r v -> Maybe (Mono v)
lt p = snd <$> lm p

-- | leading coefficient
lc :: Poly r v -> Maybe r
lc p = fst <$> lm p

-- | leading monomial
lm :: Poly r v -> Maybe (r, Mono v)
lm p = case M.maxViewWithKey $ p ^. unPoly of 
    Nothing -> Nothing
    Just ((k,v), _) -> Just (v,k)

-- | reductum
red p = case M.maxViewWithKey $ p ^. unPoly of 
    Nothing -> Nothing
    Just ((k,v), q) -> Just $ Poly { _unPoly = q }

lmRed :: Poly r v -> Maybe ((r, Mono v), Poly r v)
lmRed p = case M.maxViewWithKey $ p ^. unPoly of 
    Nothing -> Nothing
    Just ((k,v), q) -> Just ((v,k), Poly { _unPoly = q })

