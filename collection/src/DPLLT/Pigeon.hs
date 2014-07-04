-- | see http://www.cs.cmu.edu/~emc/15414-f11/assignments/hw2.pdf

module DPLL.Pigeon where

import DPLL.Data
import DPLL.Solve
import DPLL.Trace (modus0)

pigeon :: Int -> Int -> CNF
pigeon pigs holes = 
    let encode p h = Literal $ (p - 1) * holes + h
    in     do p <- [ 1 .. pigs ] 
              return $ do h <- [ 1 .. holes ] ; return $ encode p h
        ++ do h <- [ 1 .. holes ] 
              p <- [ 1.. pigs ] ; q <- [p+1 .. pigs] 
              return [ opposite $ encode p h, opposite $ encode q h ]

    
