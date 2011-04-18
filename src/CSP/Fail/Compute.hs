module CSP.Fail.Compute where

import CSP.STS.Type

import Autolib.NFA hiding ( alphabet )
import qualified Autolib.NFA

import Autolib.Symbol
import Autolib.Size
import Autolib.Hash
import Autolib.Set
import qualified Autolib.Relation as R

import qualified Data.Set as S
import qualified Data.Map as M
import Autolib.ToDoc

import Control.Monad ( guard )

failure_trace s ( w, r ) = 
    let tau = R.trans $ R.make $ hidden s
        taus p path = (p, path) : do
            q <- S.toList $ R.images tau p 
            return (q, (Nothing, q) : path )
        real = M.fromList $ do
            a <- S.toList $ alphabet s
            let r = R.make $ do 
                  (p,b,q) <- visible s ; guard $ a == b ; return (p,q)
            return ( a , r )
        rejects p = do
            guard $ S.null $ R.images tau p
            return $ S.filter ( \ x -> S.null $ R.images ( real M.! x ) p )
                   $ alphabet s
        run w p path = case w of
            [] -> taus p path
            x:xs -> do
                (p1, path1) <- taus p path
                p2 <- S.toList $ R.images ( real M.! x ) p1
                run xs p2 $ (Just x,p2) : path1
        targets = do 
            ( p, path ) <- run w (start s) []   
            r <- rejects p
            return ( (p, path), r )
        display ( (p, path), r) = toDoc ( reverse path, r )
    in  if null targets 
        then Left $ text "Nein. Es gibt keine Ablehnung zu dieser Spur."
        else case filter ( \ i -> snd i == r ) targets of
            [] -> Left $ text "Nein. Eine Realisierung einers anderen Ablehnung mit dieser Spur ist:" 
                       </> display ( head targets )
            ok -> Right $ text "Ja. Eine Realisierung dieser Ablehnung ist:"
                       </> display ( head ok )
    

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
