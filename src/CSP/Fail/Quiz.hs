module CSP.Fail.Quiz where

import CSP.Roll
import CSP.Step

import qualified  CSP.STS.Roll
import CSP.Fail.Compute
import CSP.STS.Type
import CSP.STS.Dot

import Autolib.NFA hiding ( symdiff, cross, alphabet )
import Autolib.NFA.Ops ( cross )
import Autolib.NFA.Det
import Autolib.NFA.Shortest

import qualified Data.Set as S
import Data.List ( maximumBy )
import Data.Ord ( comparing )
import Control.Monad ( forM )

roll sigma s tries = do
    its <- forM [ 1 .. tries ] $ \ k -> do
        out @ ( a, b, ss ) <- single sigma s
        let quality = if null ss then -1 
                      else minimum $ map length ss
        return ( quality, out )
    return $ snd $ maximumBy ( comparing fst ) its    

single sigma s = do
    -- a <- fmap sts $ roll_guarded_rightlinear sigma s 
    -- b <- fmap sts $ roll_guarded_rightlinear sigma s
    a <- CSP.STS.Roll.roll [ 1 .. s ] sigma
    b <- CSP.STS.Roll.roll [ 1 .. s ] sigma
    let df = symdiff ( failures a ) ( failures b )
    let ss = some_shortest df
    return ( a, b, ss )    
    
symdiff a b =     
    let da = det0 $ normalize a
        db = det0 $ normalize b
        dd = ( cross da db )
           { finals = 
                S.filter ( \ (p,q) -> S.member p ( finals da )
                               /=  S.member q ( finals db ) 
                      ) $ Autolib.NFA.states dd }
    in  dd       
        
        
            
            