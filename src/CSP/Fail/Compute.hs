module CSP.Fail.Compute where

import CSP.STS.Type

import Autolib.NFA hiding ( alphabet )
import qualified Autolib.NFA

import Autolib.Symbol
import Autolib.Size
import Autolib.Hash
import Autolib.Set

import qualified Data.Set as S
import qualified Data.Map as M
import Autolib.ToDoc

{-
failures :: NFAC t s
         => STS s t 
         -> NFA ( Either (Set t) t ) (s, Bool )
-}
failures s = 
   let q = CSP.STS.Type.states s
       unstable = S.fromList 
             $ do (p,q) <- hidden s; return p
       accepts = M.fromListWith S.union 
             $ do (p,t,q) <- visible s  
                  return (p, S.singleton t)
       links =   do (p,t,q) <-  visible s
                    return ((p,False), Right t,(q,False))
              ++ do p <- S.toList $ S.difference q unstable
                    let rej = S.difference ( CSP.STS.Type.alphabet s )
                            $ M.findWithDefault S.empty 
                                 p accepts
                    return ( (p,False), Left rej,(p,True)) 
       alpha = S.fromList $ do (p,t,q) <- links ; return t
       a =  NFA 
           { nfa_info = text "Ablehnungsmenge"
           , Autolib.NFA.alphabet = alpha
           , Autolib.NFA.states = S.fromList 
               $ S.toList q >>= \ q -> 
                   [ (q,False), (q,True ) ]
           , starts = S.singleton ( start s, False )
           , finals =  S.fromList 
               $ S.toList q >>= \ q -> [ (q,True ) ]
           , trans = collect links
           } 
   in add_epsilons a $ do 
         (p,q) <- hidden s    
         return ( (p,False) , (q,False))

instance (Symbol l, Symbol r) => Symbol ( Either l r )
instance (Symbol s) => Symbol ( Set s )
  
instance Size ( Either l r ) where size _ = 1
