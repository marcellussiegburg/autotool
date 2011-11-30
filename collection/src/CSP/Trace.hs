module CSP.Trace where

import CSP.Syntax
import CSP.STS
import CSP.Step

import Autolib.NFA
import qualified Data.Set as S
import Autolib.ToDoc

auto p = 
   let s = sts p
       q = CSP.STS.states s
       a =  NFA 
           { nfa_info = text "Spursprache"
           , Autolib.NFA.alphabet = CSP.Syntax.alphabet p 
           , Autolib.NFA.states = q
           , starts = S.singleton $ start s
           , finals = q                      
           , trans = collect $ visible s          
           } 
   in add_epsilons a $ hidden s    

