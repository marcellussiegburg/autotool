module Network.Counting.Semantics where

import Network.Counting.Data

import Autolib.Reporter
import Autolib.ToDoc

import qualified Data.Map as M

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


