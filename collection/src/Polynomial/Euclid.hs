{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}

module Polynomial.Euclid where

import Polynomial.Pattern
import Polynomial.Type
import Polynomial.Class

import qualified Prelude
import Prelude hiding 
    ( Num (..), (/), Integer, Rational(..)
    , null, gcd, divMod, div, mod )

import Autolib.Reporter hiding ( run, execute )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Challenger.Partial
import Inter.Types

import Data.Typeable
import Data.Maybe
import Data.List ( tails )
import Control.Applicative ((<$>),(<*>))

data Euclid_Sudoku dom = Euclid_Sudoku dom deriving Typeable

instance Dom d => Show (Euclid_Sudoku d) where
    show (e :: Euclid_Sudoku d) = 
        "Euclid_Sudoku" ++ name (undefined :: d)

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

instance Dom (Poly Rational Identifier) where
    name _ = "poly"
    explain _  = vcat
        [ text "domain: polynomials Q[x]"
        , text "norm: degree"
        ]

data Step r = Step { quotient :: r, remainder :: r }
    deriving Typeable

instance Size (Step r) where size _ = 1

derives [makeReader, makeToDoc] [''Step]

instance (Dom v, Euclidean_Ring v, Pattern (Step p)
   , Base (Step p) ~ (Step v), Reader v, Reader p, ToDoc v, ToDoc p ) 
    => Partial (Euclid_Sudoku v) [Step p] [Step v] where

    describe (_ :: Euclid_Sudoku v) ps = vcat
        [ text "Fill in all the holes (_) to obtain"
        , text "a correct execution sequence of Euclid's algorithm in"
        , explain (undefined :: v)
        , toDoc ps
        ]

    initial _ ps = ps >>= ( maybeToList . base )

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
    base s = Step <$> (base $ quotient s) 
                  <*> (base $ remainder s)


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
      ( [ Step { quotient = This (This 0 :+ This 0), remainder = This (This 21 :+ This 11) }
        , Step { quotient = This (This 0 :+ This 0), remainder = Any     }
        , Step { quotient = Any   , remainder = Any }
        , Step { quotient = This (Any :+ This 1), remainder = This  (This 0 :+ This 0) }
        ] :: [ Step (Patch (Complex(Patch Integer) )) ] )

