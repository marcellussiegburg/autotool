{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DatatypeContexts #-}


module Regular.Logic where

import Autolib.Logic
import Autolib.Logic.Formula.FO

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size
import Data.Typeable

import qualified Data.Set as S
import Autolib.Set

import Control.Monad ( forM_ )

data Property = 
	        Max_Size Int
              | Max_Quantifier_Depth Int
	      | Alphabet ( S.Set Char )
     deriving ( Typeable )

derives [makeReader, makeToDoc] [''Property]

example :: [ Property ]
example = [ Max_Size 50
	  , Alphabet $ mkSet "ab"
          , Max_Quantifier_Depth 4
	  ]

tests ps a = forM_ ps $ \ p -> test p a


test (Max_Size s) f = do
    let g = size f
    assert ( g <= s ) 
	   $ text "Größe des Formel" <+> parens ( toDoc g ) <+> text "ist höchstens" <+> toDoc s <+> text "?"

test (Max_Quantifier_Depth s) f = do
    let g = qdepth f
    assert ( g <= s ) 
	   $ text "Quantortiefe der Formel" <+> parens ( toDoc g ) <+> text "ist höchstens" <+> toDoc s <+> text "?"

test (Alphabet s) f = do
    let a = letters f
    assert (S.isSubsetOf a s) 
        $ text "Alphabet der Formel" <+> parens ( toDoc a ) <+> text "ist Teilmenge von" <+> toDoc s <+> text "?"

letters (Formula f) = lt f

lt f = case f of
    Forall u -> lt ( u undefined )
    Exists u -> lt ( u undefined )
    Not g -> lt g
    Or g h -> S.union ( lt g) (lt h)
    And g h -> S.union ( lt g) (lt h)
    Implies g h -> S.union ( lt g) (lt h)
    Letter c y -> S.singleton c
    _ -> S.empty

qdepth (Formula f) = qd f

qd f = case f of
    Forall u -> 1 + qd ( u undefined )
    Exists u -> 1 + qd ( u undefined )
    Not g -> qd g
    Or g h -> max (qd g) (qd h)
    And g h -> max (qd g) (qd h)
    Implies g h -> max (qd g) (qd h)
    _ -> 0

