module Network.Counting.Semantics where

import Network.Counting.Data

import Autolib.Reporter
import Autolib.ToDoc



sequential :: Network -> [Wire] 
           -> Reporter (State, [Wire])
sequential net ws = 
    let handle [] s = return (s, [])
        handle (w:ws) s0 = do
            (s1, out) <- step w s0
            (s2, outs) <- handle ws s1
            return (s2, out : outs)
    in  handle ws $ start net

start :: Network -> State 
start (Network bs) = State $ zip bs $ repeat Up

-- | only for sequential execution
step :: Wire
     -> State 
     -> Reporter ( State , Wire )
step w (State []) = do
    return ( State [], w )
step w (State (here@(B top bot, st):later)) = 
    if w == top || w == bot
    then do
        let (w',st') = case st of
                Up -> (top, Down)
                Down -> (bot, Up)
        (State later', out) <- step w' (State later)
        let here' = (B top bot, st')    
        return (State $ here' : later', out )
    else do
        (State later', out) <- step w (State later)
        return (State $ here : later' , out )

