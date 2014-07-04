{-# language DeriveDataTypeable #-}

module Fourier_Motzkin.Data where

import Data.Ratio
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Typeable

-- | linear function of a set of variables.
-- the absolute coefficient is the value for the key Nothing. 
-- invariant: all values in the map are non-zero
-- (if the coefficient is zero, then the key should not be in the map)
data Linear v = Linear (M.Map (Maybe v) Rational) 
    deriving (Eq, Ord, Typeable)

linf = Linear $ M.fromList [(Nothing, 5%2),(Just "x",-3%7), (Just "y", 4%1)]
ling = Linear $ M.fromList [(Nothing, 1%2),(Just "x", 2%7), (Just "y", 2%1)]

absolute (Linear m) = M.findWithDefault 0 Nothing m
coefficient v (Linear m) = M.findWithDefault 0 (Just v) m

variables (Linear m) = S.fromList $ catMaybes $ M.keys m

contains v (Linear m) = M.member (Just v) m

scale q (Linear m) = Linear $ M.map (* q) m

plus (Linear p) (Linear q) = 
    Linear $ M.filter (/= 0) $ M.unionWith (+) p q

minus f g = plus f ( scale (-1) g )

removeKey x (Linear m) = Linear $ M.filterWithKey ( \ k v -> k /= Just x) m

data Atom v = NonNegative { linear :: Linear v }
            | Positive    { linear :: Linear v }
    deriving (Eq, Ord, Typeable)

atom s lin = ( case s of True -> Positive ; False -> NonNegative ) lin

strict a = case a of NonNegative {} -> False ; Positive {} -> True

type Constraint v = [ Atom v ]

