
{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module CSP.Trace where

import CSP.Syntax
import CSP.STS

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Exp
import Autolib.Exp.Inter
import Autolib.NFA
import Autolib.NFA.Epsilon

import NFA.Analyse
import NFA.AI

import Data.Typeable

import Challenger.Partial
import Autolib.Reporter 
import Autolib.Size
import Inter.Types
import Inter.Quiz

data CSP_Trace = CSP_Trace deriving ( Typeable )

instance OrderScore CSP_Trace where
    scoringOrder _  = Increasing
    
$(derives [makeReader, makeToDoc] [''CSP_Trace])    

auto p = 
   let s = sts p
       q = CSP.STS.states s
       a =  NFA 
           { nfa_info = text "Spursprache"
           , states = q
           , start = S.singleton $ start s
           , final = q                      
           , trans = collect $ visible s          
           } 
   in add_epsilons a $ hidden s    

instance Partial CSP_Trace ( Process Char ) where 
    describe p i = vcat 
        [ text "Gesucht ist ein regulärer Ausdruck"
        , text "für die Spursprache des Prozesses"  
        , nest 4 $ toDoc p
        ]  
    initial p i = 
        Autolib.Exp.Example.example (alphabet i)
    partial p i b = do
        ist_einfach b
    total p i b = do 
        total NFA.Analyse.Analyse 
          ( AI { name = "Spursprache" , automat = auto i } )
          b

make_direct :: Make
make_direct = direct CSP_Trace CSP.Syntax.example1
