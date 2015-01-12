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

data Factor v = Factor { _var :: ! v, _expo :: ! Integer } 
    deriving (Typeable)

factor v e = Factor { _var = v, _expo = e }

$(makeLenses ''Factor)

instance Eq v => Eq (Factor v) where
  {-# specialize instance Eq (Factor Identifier) #-}
  (==) = (==) `on` ( \ f -> ( _var f, _expo f ) )

instance Ord v => Ord (Factor v) where
  {-# specialize instance Ord (Factor Identifier) #-}
  compare = compare `on` ( \ f -> ( _var f, _expo f ) )


-- * monomials like "x^5 z^2"

-- | invariant: store only factors with nonzero exponents.
-- implementation note: total_degree comes first,
-- and this is used in the derived Ord instance
-- (which then gives graded-lexicographic)
data Mono v = Mono { _total_degree :: ! Integer
                   , _unMono :: ! ( M.Map v Integer )
                   }
    deriving (Typeable)

$(makeLenses ''Mono)

instance Eq v => Eq (Mono v) where
  {-# specialize instance Eq (Mono Identifier) #-}
  (==) x y = {-# SCC "instance-Eq-Mono" #-}
    ((==) `on` ( \ m -> ( _total_degree m , _unMono m ))) x y

instance Ord v => Ord (Mono v) where
  {-# specialize instance Ord (Mono Identifier) #-}
  compare x y = {-# SCC "instance-Ord-Mono" #-}
    -- (compare `on` ( \ m -> ( _total_degree m , _unMono m ))) x y
    case compare (_total_degree x) (_total_degree y) of
      EQ -> compare (_unMono x) (_unMono y)
      c -> c

type instance IxValue (Mono v) = Integer
type instance Index (Mono v)  = v
instance Ord v => Ixed (Mono v) where 
    ix k = unMono  . ix k 

nullMono m = M.null $ m ^. unMono

divMono :: Ord v => Mono v -> Mono v -> Mono v
divMono m n =
    let d = M.unionWith (+) (m ^. unMono)
          $ M.map negate (n ^. unMono)
    in  if M.null $ M.filter (< 0) d
        then mono $ map (\(k,v) -> Factor k v) $ M.toList d
        else error "Polynomial.data.divMono: exponent<0"

mono :: Ord v => [Factor v] -> Mono v
mono fs = Mono 
    { _unMono = M.filter (/= 0) $ M.fromListWith (+) $ do 
        f <- fs 
        return (f ^. var , f ^. expo )
    , _total_degree = sum $ map ( ^. expo ) fs
    }

factors :: Mono v -> [ (v,Integer) ]
factors m =  M.toList $ m ^. unMono

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

variable v = poly [(1, mono [ Factor {_var=v, _expo=1 } ])]
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

