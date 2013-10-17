{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveDataTypeable #-}

module Regular.Top where

import Regular.Type

import Autolib.NFA.Eq


import Autolib.NFA ( NFA )
import qualified NFA.Property

import Autolib.Exp ( RX )
import qualified Exp.Property
import qualified Autolib.Exp.Inter as E


import Inter.Types

import Autolib.ToDoc
import Autolib.Reader
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

data (RegularC from, RegularC to ) => Config from to = Config from [ Property to ] deriving (   Typeable )

derives [makeReader, makeToDoc] [''Config]

instance ( RegularC from, RegularC to ) => C.Verify ( Regular from to ) (Config from to) where
    verify p ( Config given spec ) = return ()

instance ( RegularC from, RegularC to ) => C.Partial ( Regular from to ) ( Config from to) to where

    report p  ( Config given spec)  = do
        inform $ vcat
            [ text "Gegeben ist" <+> bestimmt given , nest 4 $ toDoc given ]
        inform $ vcat 
            [ text "Gesucht ist" <+> unbestimmt given
            , text "mit folgenden Eigenschaften"
            , nest 4 $ vcat (map toDoc spec)
            ]

    initial p ( Config given props  ) = 
        Regular.Type.initial  props

    partial p ( Config from props ) to = do
        validate props to

    total p ( Config from props  ) to = do
        alpha <- alphabet ( to `asTypeOf` undefined ) props 
        flag <- nested 4 $ do
             f <- semantics alpha from
             t <- semantics alpha to
             equ ( informed ( text "Sprache (gegeben)" ) f )
                 ( informed ( text "Sprache (gesucht)" ) t )
        when (not flag) $ reject $ text ""


instance ( RegularC from, RegularC to, Size to ) => C.Measure ( Regular from to ) ( Config from to ) to where
    measure p (Config from props ) to = fromIntegral $ size to

make_exp2nfa :: Make
make_exp2nfa = direct 
    ( Regular :: Regular (RX Char) (NFA Char Int) )
    ( Config (read  "a (a+b)^* b" )
             NFA.Property.example 
       :: Config (RX Char) (NFA Char Int) )

make_nfa2exp :: Make
make_nfa2exp = direct 
    ( Regular :: Regular (NFA Char Int)(RX Char) )
    ( Config (E.inter E.std $ read  "a (a+b)^* b" )
             Exp.Property.example 
       :: Config (NFA Char Int) (RX Char)  )

