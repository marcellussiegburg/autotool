-- | this module contains data types
-- that are needed to write the API for polynomials.
-- implementors of API should not rely on their efficiency (?)

{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}
    
module Polynomial.Base where

import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, null, gcd, divMod, div, mod )

import Polynomial.Class 
       
import Data.Typeable
import Control.Lens

import Control.Monad
import Data.Function (on)


type Expo = Int

data Factor v = Factor { _var :: ! v, _expo :: ! Expo } 
    deriving (Typeable, Eq, Ord)


factor v e | e > 0 = Factor { _var = v, _expo = e }

$(makeLenses ''Factor)

data Mono v =
     Mono { _total_degree :: ! Expo
          , _unMono :: ! [(v, Expo)] -- always decreasing
          }
    deriving (Typeable, Eq)

$(makeLenses ''Mono)

newtype Reverse a = Reverse a deriving Eq
instance Ord a => Ord (Reverse a) where
   compare (Reverse x) (Reverse y) = compare y x
             
-- should give graded reverse lexicographic             
instance Ord v => Ord (Mono v) where
  compare = compare `on` \ m ->
    ( m ^. total_degree
    , Reverse $ m ^. unMono
    )

is_decreasing m =
   let ves = m ^. unMono 
   in  and $ zipWith (\ (v,e) (w,f) -> v > w ) ves $ drop 1 ves

expos_are_positive m =
   and $ map ( \(v,e) -> e > 0 ) $ m ^. unMono           

mvalid m = is_decreasing m && expos_are_positive m
   
factors m = map ( \ (v,e) -> factor v e ) $ m ^. unMono

mono :: Ord v => [Factor v] -> Mono v
mono fs = Mono
    { _total_degree = Prelude.sum $ map ( ^. expo ) fs
    , _unMono = msort $ map ( \ f -> (f ^.var, f ^.expo) ) fs
    }

monoFromDecreasing :: Ord v => [Factor v] -> Mono v
monoFromDecreasing fs = Mono
    { _total_degree = Prelude.sum $ map ( ^. expo ) fs
    , _unMono = map ( \ f -> (f ^.var, f ^.expo) ) fs
    }

nullMono m = Prelude.null $ m ^. unMono

-- | return the quotient, if it exists
divMono :: (MonadPlus m, Ord v) => Mono v -> Mono v -> m (Mono v)
divMono m1 m2 = do
  let d = merge (m1 ^. unMono)
        $ over (mapped . _2) negate (m2 ^. unMono)
  guard $  all ( \ (v,e) -> e >= 0 ) d
  return $ monoFromDecreasing $ map (uncurry factor) d

dividesMono m1 m2 =
  let d = merge (m2 ^. unMono)
        $ over (mapped . _2) negate (m1 ^. unMono)
  in  all ( \ (v,e) -> e >= 0 ) d

lcmMono :: Ord v => Mono v -> Mono v -> Mono v
lcmMono m1 m2 = 
  let d = mergeDescWith max (m1 ^. unMono) (m2 ^. unMono)
  in  monoFromDecreasing $ map (uncurry factor) d

type Term r v = (r, Mono v)

msort [] = [] ; msort [x] = [x]
msort xs =
  let (lo,hi) = splitAt (Prelude.div (length xs) 2) xs
  in  merge (msort lo) (msort hi)

{-# inlineable merge #-}

merge xs ys = mergeDesc xs ys

mergeDesc xs ys = mergeDescWith (+) xs ys

-- | largest comes first

{-# inlineable mergeDescWith #-}

mergeDescWith f [] ys = ys
mergeDescWith f xs [] = xs
mergeDescWith f (x:xs) (y:ys) = case compare (fst x) (fst y) of
  GT -> x : mergeDescWith f xs (y:ys)
  LT -> y : mergeDescWith f (x:xs) ys
  EQ -> let v = f (snd x) (snd y)
        in  if v == zero then mergeDescWith f xs ys
            else (fst x, v) : mergeDescWith f xs ys 

multMono p q = monoMult p q  

monoMult p q = Mono 
    { _unMono = mergeDesc (p ^. unMono) (q ^. unMono) 
    , _total_degree = p ^. total_degree + q ^. total_degree
    }

   
