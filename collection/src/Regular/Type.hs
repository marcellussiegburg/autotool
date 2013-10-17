-- | several options to specify regular languages

{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}

module Regular.Type where

import Autolib.Exp

import qualified Autolib.NFA as A
import qualified Autolib.NFA.Example 
import qualified NFA.Property as A

import qualified Autolib.Exp as E
import qualified Autolib.Exp.Example 
import qualified Exp.Property as E
import qualified Autolib.Exp.Inter as E

-- import Autolib.Logic

import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Reader
import Autolib.Size
import Inter.Types

import Data.Typeable
import qualified Data.Set as S

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

    validate p a = return ()
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

    validate p e = return ()
    semantics alpha e = 
        return $ E.inter (E.std_sigma $ S.toList alpha) e

