module Network.Counting.Semantics where

import Network.Counting.Data
import Network.Counting.Picture

import Autolib.Reporter
import Autolib.ToDoc

import qualified Data.Map as M

singles :: (Wire, Wire)   
        -> Network 
        -> [Wire] 
        -> Reporter ()
singles (lo, hi) net ws = do
    let handle net ws expect = case ws of 
            [] -> return ()
            w : ws -> do
                let (net', w') = single net w
                inform $ besides [ picture net 
                         , toDoc w <+> text "=>" 
                                   <+> toDoc w'
                         , picture net'
                         ]
                when (w' /= expect) $ reject
                    $ text "erwartete Ausgabe:"
                       <+> toDoc expect
                let expect' = if expect == hi then lo
                              else succ expect
                handle net' ws expect'
    handle net ws lo

-- | one token is routed through the network.
-- the resulting network has some gates flipped.
single :: Network -> Wire
       -> ( Network, Wire)
single (Network bs) w = 
    let handle [] w = ( [], w )
        handle ((up,down) : bs) w = 
            if w == up || w == down
            then let (bs', out) = handle bs up
                 in  ((down,up) : bs', out )
            else let (bs', out) = handle bs w
                 in  ((up,down) : bs', out )
        ( bs', out ) = handle bs w
    in  ( Network bs', out )

-- | route a set of tokens 
bulk :: Network 
     -> M.Map Wire Int
     -> M.Map Wire Int
bulk (Network bs) m = 
    let handle [] m = m
        handle ((up,down) : bs) m = 
            let total = M.findWithDefault 0  up   m
                      + M.findWithDefault 0  down m
                lo = div total 2 ; hi = total - lo
            in  handle bs 
                  $ M.insert down lo 
                  $ M.insert up hi m
    in  handle bs m


