{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}

module Regular.Top where

import Regular.Type

import Autolib.NFA.Eq


import qualified Exp.Property
import Autolib.NFA ( NFA )
import Autolib.Exp ( RX )

import Inter.Types

import Autolib.ToDoc
import Autolib.Hash

import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter
import Autolib.Informed
import Autolib.Size



data Regular from to = Regular
    deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore ( Regular from to ) where
    scoringOrder _ = Increasing

instance C.Verify ( Regular from to ) ( from, [ Property from ] ) where
    verify p ( given, spec ) = return ()

instance C.Partial ( Regular from to ) ( from, [ Property from ] ) to where

    report p  ( given, spec)  = do
        inform $ vcat
            [ text "Gegeben ist" <+> bestimmt given , nest 4 $ toDoc given ]
        inform $ vcat 
            [ text "Gesucht ist" <+> unbestimmt given
            , text "mit folgenden Eigenschaften"
            , nest 4 $ vcat (map toDoc spec)
            ]

    initial p ( from, props ) = Regular.Type.initial props

    partial p ( from, props ) to = do
        validate props to

    total p ( from, props ) to = do
        alpha <- alphabet props
        flag <- nested 4 
             $ equ ( informed ( text "Sprache (gegeben)" ) $ semantics alpha from ) 
                   ( informed ( text "Sprache (gesucht)" ) $ semantics alpha to   )
        when (not flag) $ reject $ text ""


instance C.Measure ( Regular from to ) ( from, [ Property from ] ) to where
    measure p (from, props ) to = fromIntegral $ size to

make_exp2nfa :: Make
make_exp2nfa = direct ( Regular :: Regular (RX Int) (NFA Char Int) )
     ( read  "a (a+b)^* b" , Exp.Property.example )

