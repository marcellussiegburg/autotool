-- | several options to specify regular languages

{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}

module Regular.Type where

import Autolib.Exp

import qualified Autolib.NFA as A
import qualified Autolib.NFA.Example 
import qualified NFA.Property as A
import qualified NFA.Test as A

import qualified Autolib.Exp as E
import qualified Autolib.Exp.Example 
import qualified Exp.Property as E
import qualified Exp.Test as E
import qualified Autolib.Exp.Inter as E

import qualified Autolib.Logic.Formula.FO as L
import qualified Autolib.Logic as L
import qualified Regular.Logic as L

import qualified Grammatik as G
import qualified Grammatik.Property as G

import Autolib.ToDoc
import Autolib.Reporter
import qualified Autolib.Reporter.Checker as C
import Autolib.Reader
import Autolib.Size
import Inter.Types

import Data.Typeable
import qualified Data.Set as S
import Control.Monad (forM_ )

class ( ToDoc a, Reader a, Size a , Typeable a
      , ToDoc ( Property a ), Reader (Property a)
      ) => RegularC a where
    type Property a
    alphabet :: a -- ^ dummy, just for typing
                -> [ Property a ] 
             -> Reporter ( S.Set Char )
    bestimmt :: a -> Doc
    unbestimmt :: a -> Doc
    initial :: [ Property a ] -> a
    validate :: [ Property a ] -> a -> Reporter ()
    semantics :: S.Set Char -> a -> Reporter ( A.NFA Char Int )

instance RegularC (A.NFA Char Int) where

    type Property (A.NFA Char Int) = A.Property Char 

    bestimmt _ = text "der endliche Automat" 
    unbestimmt _ = text "ein dazu äquivalenter endlicher Automat" 

    alphabet _ props = do
        let [ alpha ] = do A.Alphabet a <- props ; return a
        return  alpha
    
    initial props = 
        let [ alpha ] = do A.Alphabet a <- props ; return a    
        in  Autolib.NFA.Example.example_sigma alpha

    validate props a = A.tests props a
    semantics alpha a = return a


instance RegularC (E.RX Char ) where

    type Property (E.RX Char ) = E.Property Char 

    bestimmt _ = text "der reguläre Ausdruck" 
    unbestimmt _ = text "ein dazu äquivalenter regulärer Ausdruck" 

    alphabet _ props = do
        let [ alpha ] = do E.Alphabet a <- props ; return a
        return  alpha

    initial props = 
        let [ alpha ] = do E.Alphabet a <- props ; return a
        in  Autolib.Exp.Example.example alpha

    validate props e = E.tests props e
    semantics alpha e = 
        return $ E.inter (E.std_sigma $ S.toList alpha) e


instance RegularC L.Formula where

    type Property L.Formula = L.Property 

    bestimmt _ = text "die Formel" 
    unbestimmt _ = text "eine dazu äquivalente Formel" 

    alphabet _ props = do
        let [ alpha ] = do L.Alphabet a <- props ; return a
        return  alpha

    initial props = 
        let [ alpha ] = do L.Alphabet a <- props ; return a
        in  L.f1

    validate props e = L.tests props e
    semantics alpha e = 
        return $ L.semantics alpha e

instance RegularC G.Grammatik where

    type Property G.Grammatik = G.Property

    bestimmt _ = text "die Grammatik"
    unbestimmt _ = text "eine dazu äquivalente Grammatik"

    alphabet _ props = do
        let [ alpha ] = do G.Alphabet a <- props ; return a
        return  alpha

    initial props = 
        let [ alpha ] = do G.Alphabet a <- props ; return a
        in  G.example

    validate props g = forM_ props $ \ p -> 
        C.run (G.check p) g

    semantics alpha g = do
        let a = A.NFA { A.alphabet = alpha
                      , A.states = variablen g
                      , A.starts = start g
                      , A.finals = 