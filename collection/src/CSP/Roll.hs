module CSP.Roll where

import CSP.Syntax
import CSP.Property
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
    p <- roll Iteration_Fixpoint sigma s
    let a = auto p
        m = minimize0 $ normalize a
    print ( p, size a, size m )    
  

-- | roll a guarded, right-linear process
-- that contains a Par with non-empty synch set    
roll how_to_iterate sigma s = do
    p <- roll_free how_to_iterate sigma s
    if (case how_to_iterate of
           Iteration_Fixpoint -> CSP.Property.Guarded.ok p 
                              && CSP.Property.Right_Linear.ok p 
           Iteration_Star -> True )
       && root_looks_interesting p
       && contains_non_trivial_synch p
       then return p else roll how_to_iterate sigma s

root_looks_interesting p = case p of
    -- Par {} -> True
    Fix {} -> True
    _ -> False

contains_non_trivial_synch p = or $ do
    q @ ( Par s _ _ ) <- subs p
    -- let fs = do Fix f <- subs q ; return f
    return $ not ( null s ) -- && not ( null fs )

roll_guarded sigma s = do
    p <- roll_free Iteration_Fixpoint sigma s
    if    CSP.Property.Guarded.ok p 
       then return p else roll_guarded sigma s

roll_guarded_rightlinear sigma s = do
    p <- roll_free Iteration_Fixpoint sigma s
    if    CSP.Property.Guarded.ok p 
       && CSP.Property.Right_Linear.ok p 
       then return p else roll_guarded_rightlinear sigma s

roll_free how_to_iterate sigma s = do
   -- putStrLn "roll_free ..."
   p <- nonrec sigma s >>= fixup how_to_iterate >>= fixup how_to_iterate
   -- putStrLn "   ... roll_free"
   return p
    
fixups hti p = do              
    action <- eins [ \ q -> fixup hti q >>= fixups hti, return ]
    action p
              
topfix p = do
    b <- unstops p
    return $ Fix b

-- | add one fixpoint operator (anywhere)
fixup :: Iteration -> Process a -> IO ( Process a )
fixup hti p = case hti of
    Iteration_Fixpoint -> do
        ( f, a ) <- eins $ splits p
        b <- unstops a
        return $ f $ Fix b    
    Iteration_Star -> do
        ( f, a ) <- eins $ splits p
        return $ f $ Star a
    
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
