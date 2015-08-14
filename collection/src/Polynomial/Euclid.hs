{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}

module Polynomial.Euclid where

import Polynomial.Patch
import Polynomial.Pattern
import Polynomial.Type
import Polynomial.Class

import qualified Polynomial.Unary as U

import qualified Prelude
import Prelude hiding 
    ( map, Num (..), (/), Integer, Rational(..)
    , null, gcd, divMod, div, mod )

import Autolib.Reporter hiding ( run, execute )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Data.Typeable
import Data.Maybe
import Data.List ( tails )
import Control.Applicative ((<$>),(<*>))
import System.Random

data Euclid_Sudoku dom = Euclid_Sudoku dom deriving Typeable

instance Dom d => Show (Euclid_Sudoku d) where
    show (e :: Euclid_Sudoku d) = 
        "Euclid_Sudoku-" ++ name (undefined :: d)

instance Read (Euclid_Sudoku dom)

instance OrderScore (Euclid_Sudoku dom) where
    scoringOrder _ = Increasing

class Dom dom where
    name :: dom -> String
    explain :: dom -> Doc


instance Dom Integer where
    name _ = "integers"
    explain _ = vcat [ text "domain: integers"
                     , text "norm: absolute value"
                     ]

instance Dom (Complex Integer) where
    name _ = "gauss"
    explain _ = vcat 
        [ text "domain: Gaussian integers (a + b i)"
        , text "norm: a^2 + b^2"
        ]

instance Dom (U.Poly Rational) where
    name _ = "poly"
    explain _  = vcat
        [ text "domain: polynomials Q[x]"
        , text "norm: degree"
        ]

instance Ring r => Ring (Patch r) where
   zero = This zero
   one  = This  one
   negative _ = False

instance Size (Step r) where size _ = 1


instance (Dom d, Euclidean_Ring d, Pattern p
   , Base p ~ d, Reader d, Reader p, ToDoc d, ToDoc p ) 
    => Partial (Euclid_Sudoku d) [Step p] [Step d] where

    describe (_ :: Euclid_Sudoku v) ps = vcat
        [ text "Fill in all the holes (_) to obtain"
        , text "a correct execution sequence of Euclid's algorithm in"
        , explain (undefined :: v)
        , toDoc ps
        ]

    initial _ ps = fmap base ps

    partial _ ps vs = do
        forM_ (zip [0 :: Int ..] $ tails vs) $ \ (i, t) -> case t of
            a : b : c : _ -> do
                inform $ text "checking step i =" <+> toDoc i
                let x = remainder b * quotient c 
                      + remainder c
                assert (remainder a == x) $ vcat
                    [ text "remainder(i)" 
                      </> toDoc (remainder a)
                    , text "=="
                    , text "remainder(i+1) * quotient(i+2) + remainder(i+2)"
                      </> toDoc x
                    ]
                assert (norm (remainder b) > norm (remainder c)) $ vcat
                    [ text "norm (remainder (i+1))" 
                      </> toDoc (norm (remainder b))
                    , text ">"
                    , text "norm (remainder (i+2))"
                      </> toDoc (norm (remainder c))
                    ]
            _ -> return ()
        
    total _ ps vs = do
        case vs of
            [] -> return ()
            _ -> assert (zero == remainder (last vs)) 
               $ text "last remainder must be zero"
        assert (length ps == length vs) 
           $ text "length of pattern == length of submission"
        forM_ (zip3 [0 :: Int ..] ps vs) $ \ (k, p,v) -> silent $ do
            assert (match p v) $ vcat
                [ text "mismatch"
                , text "position:" <+> toDoc k
                , text "value:" <+> toDoc v
                , text "pattern:" <+> toDoc p
                ]

instance Pattern p => Pattern (Step p) where
    type Base (Step p) = Step (Base p)
    match s v = match (quotient s) (quotient v)
            &&  match (remainder s) (remainder v)
    base s = Step (base $ quotient s) (base $ remainder s)
    default_ (s :: Step p) = 
        Step (default_ (undefined :: p))
             (default_ (undefined :: p))
    robfuscate s = Step <$> robfuscate (quotient s)
                        <*> robfuscate (remainder s)
    inject s = Step (inject $ quotient s) 
                    (inject $ remainder s)

make_fixed_integer :: Make
make_fixed_integer = 
    direct (Euclid_Sudoku (undefined::Integer)) 
      ( [ Step { quotient = This 0, remainder = This 21 }
        , Step { quotient = This 0, remainder = Any     }
        , Step { quotient = Any   , remainder = This  6 }
        , Step { quotient = This 2, remainder = Any     }
        , Step { quotient = This 2, remainder = This  0 }
        ] :: [ Step (Patch Integer ) ] )

make_fixed_gauss :: Make
make_fixed_gauss = 
    direct (Euclid_Sudoku (undefined::Complex Integer)) 
      ( [ Step { quotient = inject zero, remainder = This (This 21 :+ This 11) }
        , Step { quotient = inject zero, remainder = Any     }
        , Step { quotient = Any   , remainder = Any }
        , Step { quotient = This (Any :+ This (1::Integer)), remainder = inject zero }
        ] )

make_fixed_upoly :: Make
make_fixed_upoly = 
    direct (Euclid_Sudoku (undefined:: U.Poly Rational)) 
      ( [ ] :: [ Step ( (U.P ( Patch (Ratio Integer))
                             ( Patch Integer))) ] )

data Config p = Config 
    { number_range :: (Integer, Integer)
    , max_degree :: Integer -- ^ only for polynomial domain
    , num_terms :: Int -- ^ only for polynomial domain
    , num_steps :: Int
    , take_best_of :: Int
    } deriving Typeable

derives [makeReader, makeToDoc] [''Config]

class Gen p where gen :: Config a -> IO p

instance Gen Integer where 
    gen conf = randomRIO $ number_range conf
instance Gen (Complex Integer) where
    gen conf = (:+) 
           <$> (randomRIO $ number_range conf)  
           <*> (randomRIO $ number_range conf)
instance Gen Rational where
    gen conf = (%)
           <$> (randomRIO $ number_range conf) 
           <*> (Prelude.succ <$> Prelude.abs <$> randomRIO ( number_range conf) )

instance Gen (U.Poly Rational) where
    gen conf = U.poly <$> replicateM (num_terms conf) 
        ( (,) <$> gen conf <*> randomRIO (0, max_degree conf)  )

ci :: Config (Patch Integer)
ci = Config 
   { number_range = (10, 1000) , max_degree = 0, num_terms = 0 
   , num_steps = 5 
   , take_best_of = 1000 
   }

cg :: Config (Patch (Complex (Patch Integer)))
cg = Config 
   { number_range = (-100, 100) , max_degree = 0, num_terms = 0 
   , num_steps = 5 
   , take_best_of = 100
   }


cp :: Config (U.P (Patch (Ratio  Integer)) 
                  (Patch Integer))
cp = Config 
   { number_range = (-2, 2) , max_degree = 10
   , num_terms = 4
   , num_steps = 5 
   , take_best_of = 100
   }


roll_best :: Int -> (a -> Int)  -> (IO a) -> IO a
roll_best k f gen = do
    let h best todo = 
            if todo > 0 then do 
                x <- gen 
                h (if f x < f best then x else best) (pred todo)
            else return best
    x <- gen ; h x k

instance ( Base p ~ v, Gen v, Reader p, Reader v 
         , ToDoc p, ToDoc v, Euclidean_Ring v, Pattern p
         ) => Generator (Euclid_Sudoku v) (Config p) ([ Step p ],[Step v ]) where
    generator _ conf key = roll_best (take_best_of conf) 
        ( \ (ps,ss) -> Prelude.abs (length ss Prelude.- num_steps conf) )
        $ do
            a <- gen conf ; b <- gen conf
            let ss = gcd_steps a b
            ps <- forM ss robfuscate
            return (ps, ss)

instance Project (Euclid_Sudoku dome) (a,b) a where
    project _ (a,b) = a

make_quiz_integer :: Make
make_quiz_integer = 
    quiz (Euclid_Sudoku (undefined::Integer)) ci

make_quiz_gauss :: Make
make_quiz_gauss = 
    quiz (Euclid_Sudoku (undefined::Complex Integer)) cg

make_quiz_upoly :: Make
make_quiz_upoly = 
    quiz (Euclid_Sudoku (undefined::U.Poly Rational)) cp

