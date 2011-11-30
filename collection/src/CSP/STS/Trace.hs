-- | trace semantics

module CSP.STS.Trace where

import CSP.STS.Type

import Autolib.NFA
import qualified Data.Set as S
import Autolib.ToDoc

traces s = 
   let 
       q = CSP.STS.Type.states s
       a =  NFA 
           { nfa_info = text "Spursprache"
           , Autolib.NFA.alphabet = CSP.STS.Type.alphabet s
           , Autolib.NFA.states = q
           , starts = S.singleton $ start s
           , finals = q                      
           , trans = collect $ visible s          
           } 
   in add_epsilons a $ hidden s    

