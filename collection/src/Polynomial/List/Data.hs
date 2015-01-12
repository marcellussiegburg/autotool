{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module Polynomial.List.Data where

import Polynomial.Class ( zero )
import Data.Typeable
import Control.Lens

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

factors m = m ^. unMono

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

constant r =
  Poly { _unPoly = [ (mono [], r) | r /= zero ]
       , _absolute = r
       }

null p = Prelude.null $ p ^. unPoly

terms p = map ( \(m,c) -> (c,m) ) $ p ^. unPoly
nterms p = length $ p ^. unPoly

