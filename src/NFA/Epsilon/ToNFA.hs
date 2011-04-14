module NFA.Epsilon.ToNFA where

import qualified NFA.Epsilon.Data as E

import Autolib.NFA 
import Autolib.NFA.Epsilon
import qualified Autolib.Relation 

-- | the real part of the automaton (all non-eps transitions)
real :: NFAC c s => E.ENFA c s -> NFA c s
real e = NFA 
       { nfa_info = funni "Un.real" [ E.nfa_info e ]
       , alphabet = E.alphabet e                   
       , states = E.states e             
       , starts = E.starts e           
       , finals = E.finals e           
       , trans = E.trans e           
       }          

-- | compute language-equivalent automaton
toNFA :: NFAC c s => E.ENFA c s -> NFA c s
toNFA e = add_epsilons ( real e ) 
     $ Autolib.Relation.pairs $ E.epsilon_trans e

