{-# language TemplateHaskell #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}

module Fractran.Machine where

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I
import Machine.History

import qualified Autolib.Reporter.Checker as R

import Machine.Class
import Data.Ratio

import Autolib.Set
import Autolib.Size
import Autolib.ToDoc
import Autolib.Reporter
import Inter.Types 

import Control.Monad ( guard )
import Data.Typeable

type Memory = Integer
type Program = [Rational]

example :: Program
example =
  [ 17%91, 78%85, 19%51, 23%38, 29%33
  , 77%29, 95%23, 77%19,  1%17, 11%13
  , 13%11, 15%14, 15% 2, 55% 1
  ]

data State = State
  { schritt :: Int
  , memory :: Memory
  , past :: [ State ]
  } deriving (Eq, Ord, Typeable)

$(derives [makeToDoc] [''State])

instance Size Rational where size _  = 1

instance History State where history = past

instance C.Check () Program where
  check () p = do
    let wrong = filter (<= 0) p
    when (not $ null wrong) $ reject $ vcat
      [ text "alle Zahlen im Programm sollen positiv sein,"
      , text "aber diese sind es nicht:" <+> toDoc wrong
      ]

instance Compute Program State where
    depth _ = schritt
    next p s = mkSet $ step p s
    accepting p s = null $ step p s

step p s = take 1 $ do
        f <- p
        let (d,m) = divMod (memory s) (denominator f)
        guard $ m == 0
        return $  State { memory = d * numerator f
                  , schritt = succ $ schritt s
                  , past = s : past s
                  }

instance In Program Memory State where
    input_reporter p m = do
        return $ State { memory = m, schritt = 0, past = [] }
instance Out Program Memory State where
    output_reporter p s = do
        return $ memory s

primes :: [ Integer ]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (x:ys) = x : sieve (filter (\ y -> 0 /= mod y x) ys)

exponent_of :: Integer -> Integer -> Integer
exponent_of b x =
  case divMod x b of
    (d,0) -> 1 + exponent_of b d
    _ -> 0

instance Encode Memory where
    encode xs = product $ zipWith (^) primes xs
    
instance Decode Memory where
    decode m = exponent_of (primes !! 0) m

make :: Make
make = M.make $ C.Config
       { C.name = "Fractran"
       , C.conditions = [ ]
       , C.arity = 2
       , C.op = read "x1 + x2"
       , C.num_args = 10
       , C.max_arg = 20
       , C.cut = 1000
       , C.checks = [ ] :: [()]
       , C.start = example
       }


    


