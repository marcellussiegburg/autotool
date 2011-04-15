module CSP.Roll where

import CSP.Syntax
import CSP.Property.Guarded
import CSP.Property.Right_Linear
import CSP.Trace

import Autolib.NFA
import Autolib.NFA.Minimize
import Autolib.NFA.Normalize
import Autolib.Size

import Autolib.Util.Zufall
import Data.List ( sort )

expa sigma s = do
    p <- roll sigma s
    let a = auto p
        m = minimize0 $ normalize a
    print ( p, size a, size m )    
  

-- | roll a guarded, right-linear process
-- that contains a Par with non-empty synch set    
roll sigma s = do
    p <- roll_free sigma s
    if    CSP.Property.Guarded.ok p 
       && CSP.Property.Right_Linear.ok p 
       && root_looks_interesting p
       && contains_non_trivial_synch p
       then return p else roll sigma s

root_looks_interesting p = case p of
    -- Par {} -> True
    Fix {} -> True
    _ -> False

contains_non_trivial_synch p = or $ do
    q @ ( Par s _ _ ) <- subs p
    -- let fs = do Fix f <- subs q ; return f
    return $ not ( null s ) -- && not ( null fs )

roll_guarded sigma s = do
    p <- roll_free sigma s
    if    CSP.Property.Guarded.ok p 
       then return p else roll_guarded sigma s

roll_free sigma s = do
   -- putStrLn "roll_free ..."
   p <- nonrec sigma s >>= fixup >>= fixup
   -- putStrLn "   ... roll_free"
   return p
    
fixups p = do              
    action <- eins [ \ q -> fixup q >>= fixups, return ]
    action p
              
topfix p = do
    b <- unstops p
    return $ Fix b

-- | add one fixpoint operator (anywhere)
fixup :: Process a -> IO ( Process a )
fixup p = do
    ( f, a ) <- eins $ splits p
    b <- unstops a
    return $ f $ Fix b    
    
-- | replace at least one Stop by Point    
unstops a = do    
    let fs = do (f, Stop) <- splits a ; return f
    if null fs then return a else do
        f <- eins fs
        action <- eins [ unstops , return ]
        action $ f Point
                     
nonrec :: Ord a => [a] -> Int -> IO ( Process a )
nonrec sigma s = 
    if s <= 1 then unary sigma s else do
        action <- eins [ unary sigma s, binary sigma s ]
        action 
        
unary sigma s = do        
    t <- if s > 2 then nonrec sigma (s-1) 
         else return Stop
    a <- eins sigma
    return $ Pre a t
    
binary sigma s = do    
    sl <- randomRIO ( 1, s-2 )
    let sr = s - 1 - sl
    l <- nonrec sigma sl    
    r <- nonrec sigma sr
    action <- eins [ return Int, return Ext, return Seq
                   , do k <- randomRIO ( 0, length sigma )
                        s <- selektion k sigma
                        return $ Par $ sort s
                   ] 
    f <- action ;           
    return $ f l r          
