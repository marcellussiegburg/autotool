module CSP.Fail.Quiz where

import CSP.Roll
import CSP.Step


import qualified  CSP.STS.Roll
import CSP.Fail.Compute
import CSP.STS.Type
import CSP.STS.Dot
import CSP.STS.Trace

import Autolib.NFA hiding ( symdiff, cross, alphabet )
import qualified Autolib.NFA
import Autolib.NFA.Ops ( cross )
import Autolib.NFA.Det
import Autolib.NFA.Shortest

import qualified Data.Set as S
import Data.List ( maximumBy )
import Data.Ord ( comparing )
import Control.Monad ( forM )
import Data.Either

roll sigma s vis hid mut tries = do
    its <- forM [ 1 .. tries ] $ \ k -> do
        out @ ( a, b, (st, sf) ) <- 
            single sigma s vis hid mut
        let quality = if null sf then -1 
                      else minimum $ map length sf
        return ( (null st, quality), out )
    return $ snd $ maximumBy ( comparing fst ) its    

single sigma s vis hid mut = do
    -- a <- fmap sts $ roll_guarded_rightlinear sigma s 
    -- b <- fmap sts $ roll_guarded_rightlinear sigma s
    a <- CSP.STS.Roll.roll_reachable [ 1 .. s ] sigma vis hid
    b <- CSP.STS.Roll.mutate mut a
    let dt = symdiff ( traces a ) ( traces b )
        st = some_shortest dt

    let df = symdiff ( failures a ) ( failures b )
        sf = some_shortest df

    case sf of
        s : _ -> let ([r], w) = partitionEithers s in
             case ( failure_trace a (w,r), failure_trace b (w,r)) of
                 c @ ( Left msg1, Left msg2 ) -> error $ show c
                 c @ ( Right msg1, Right msg2 ) -> error $ show c
                 _ -> return ()
        _ -> return ()

    return ( a, b, (st, sf) )    
    
symdiff a b =     
    let co = S.union ( Autolib.NFA.alphabet a ) ( Autolib.NFA.alphabet b )
        da = det0 $ normalize $ a { Autolib.NFA.alphabet = co }
        db = det0 $ normalize $ b { Autolib.NFA.alphabet = co }
        dd = ( cross da db )
           { finals = 
                S.filter ( \ (p,q) -> S.member p ( finals da )
                               /=  S.member q ( finals db ) 
                      ) $ Autolib.NFA.states dd }
    in  dd       
        
        
            
            