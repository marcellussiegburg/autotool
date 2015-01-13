{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module Polynomial.List.Data where

import Prelude hiding (Num (..), (/), Integer, map, null)
import qualified Prelude

import Polynomial.Class 
import Polynomial.Base

import Data.Typeable
import Control.Lens

import Control.DeepSeq


-- | unPoly contains *all* terms (including absolute)
data Poly r v = Poly { unPoly :: ! [ (Mono v, r) ]
                     , absolute :: ! r
                     }
    deriving ( Eq, Typeable )


constant :: (Ord v, Ring r) => r -> Poly r v
constant r =
  Poly { unPoly = [ (mono [], r) | r /= zero ]
       , absolute = r
       }

null p = Prelude.null $ unPoly p 

terms p = Prelude.map ( \(m,c) -> (c,m) ) $ unPoly p
nterms p = length $ unPoly p


valid p = monotone (unPoly p )
  && all (monotone .  _unMono . fst ) ( unPoly p )
  && all (\ (m,c) -> c /= zero ) (unPoly p)

monotone kvs = and $
  zipWith ( \ (k1,v1) (k2, v2) -> k1 > k2 ) kvs ( drop 1 kvs )
