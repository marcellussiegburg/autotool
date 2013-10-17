-- | several options to specify regular languages

{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}

module Regular.Type where

import Autolib.Exp

import qualified Autolib.NFA as A
import qualified Autolib.NFA.Example 
import qualified NFA.Property as A

-- import Autolib.Logic

import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Reader
import Inter.Types

import qualified Data.Set as S

class RegularC a where
    type Property a
    alphabet :: [ Property a ] -> Reporter ( S.Set Char )
    bestimmt :: a -> Doc
    unbestimmt :: a -> Doc
    initial :: [ Property a ] -> Reporter a
    validate :: [ Property a ] -> a -> Reporter ()
    semantics :: S.Set Char -> a -> Reporter ( A.NFA Char Int )

instance RegularC (A.NFA Char Int) where

    type Property (A.NFA Char Int) = A.Property Char 

    bestimmt _ = text "der endliche Automat" 
    unbestimmt _ = text "ein dazu äquivalenter endlicher Automat" 

    initial props = do 
        [ alpha ] <- return $ do A.Alphabet a <- props ; return a
	return $  Autolib.NFA.Example.example_sigma alpha

    validate p a = return ()
    semantics alpha a = return a

