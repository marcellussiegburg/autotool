-- | this module contains data types
-- that are needed to write the API for polynomials.
-- implementors of API should not rely on their efficiency (?)

{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module Polynomial.Base where

import Polynomial.Class ( zero )

import Data.Typeable
import Control.Lens

import Control.DeepSeq

type Expo = Int

data Factor v = Factor { _var :: ! v, _expo :: ! Expo } 
    deriving (Typeable, Eq, Ord)

factor v e | e > 0 = Factor { _var = v, _expo = e }

$(makeLenses ''Factor)

data Mono v = Mono { _total_degree :: ! Expo
                   , _unMono :: ! [(v, Expo)]
                   }
    deriving (Typeable, Eq, Ord)

$(makeLenses ''Mono)

factors m = map ( \ (v,e) -> factor v e ) $ m ^. unMono

mono :: Ord v => [Factor v] -> Mono v
mono fs = Mono
    { _total_degree = sum $ map ( ^. expo ) fs
    , _unMono = msort $ map ( \ f -> (f ^.var, f ^.expo) ) fs
    }

nullMono m = Prelude.null $ m ^. unMono

type Term r v = (r, Mono v)

msort [] = [] ; msort [x] = [x]
msort xs =
  let (lo,hi) = splitAt (div (length xs) 2) xs
  in  merge (msort lo) (msort hi)

-- | largest comes first
merge [] ys = ys ; merge xs [] = xs
merge (x:xs) (y:ys) = case compare (fst x) (fst y) of
  GT -> x : merge xs (y:ys)
  LT -> y : merge (x:xs) ys
  EQ -> let v = snd x + snd y
        in  if v == zero then merge xs ys
            else (fst x, v) : merge xs ys                  

monoMult p q = Mono 
    { _unMono = merge (p ^. unMono) (q ^. unMono) 
    , _total_degree = p ^. total_degree + q ^. total_degree
    }
