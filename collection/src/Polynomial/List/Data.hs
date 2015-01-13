{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module Polynomial.List.Data where

import Polynomial.Class ( zero )
import Data.Typeable
import Control.Lens

import Control.DeepSeq

type Expo = Int

data Factor v = Factor { _var :: ! v, _expo :: ! Expo } 
    deriving (Typeable, Eq, Ord)

factor v e = Factor { _var = v, _expo = e }

$(makeLenses ''Factor)

data Mono v = Mono { _total_degree :: ! Expo
                   , _unMono :: ! [(v, Expo)]
                   }
    deriving (Typeable, Eq, Ord)

$(makeLenses ''Mono)

instance NFData v => NFData (Mono v) where
  rnf m = rnf (_total_degree m ) `seq` rnf ( _unMono m ) `seq` ()

factors m = map (\(v,e) -> factor v e) $ m ^. unMono

mono :: [Factor v] -> Mono v
mono fs = Mono
    { _total_degree = sum $ map ( ^. expo ) fs
    , _unMono = map ( \ f -> (f ^.var, f ^.expo) ) fs
    }

nullMono m = Prelude.null $ m ^. unMono

-- | unPoly contains *all* terms (including absolute)
data Poly r v = Poly { _unPoly :: ! [ (Mono v, r) ]
                     , _absolute :: ! r
                     }
    deriving ( Eq, Typeable )

$(makeLenses ''Poly)

instance (NFData r, NFData v) => NFData (Poly r v) where
  rnf p = rnf (_unPoly p ) `seq` rnf ( _absolute p ) `seq` ()

constant r =
  Poly { _unPoly = [ (mono [], r) | r /= zero ]
       , _absolute = r
       }

null p = Prelude.null $ p ^. unPoly

terms p = map ( \(m,c) -> (c,m) ) $ p ^. unPoly
nterms p = length $ p ^. unPoly


valid p = monotone (p ^. unPoly )
  && all (monotone .  _unMono . fst ) (p ^. unPoly )
  && all (\ (m,c) -> c /= zero ) (p ^. unPoly)

monotone kvs = and $
  zipWith ( \ (k1,v1) (k2, v2) -> k1 > k2 ) kvs ( drop 1 kvs )
