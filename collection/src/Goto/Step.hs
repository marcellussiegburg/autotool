{-# language DeriveDataTypeable #-}

module Goto.Step where

import Goto.Type
import Goto.Memory
import Goto.State

import Control.Monad ( guard, mzero )

import qualified RAM.Builtin as B

-- | liste aller direkten nachfolge-zustände
-- for deterministische maschinen hier immer länge <= 1
step :: State -> [ State ]
step s = case get_command s of
  Nothing -> mzero
  Just c -> case c of
    Inc v -> stepped $ update succ s v
    Dec v -> stepped $ update ( \ n -> max 0 (pred n) ) s v
    Assign res name args ->
      let (ar, fun) = B.get name
          inputs = map (get (memory s)) args
          output = fun inputs
      in  stepped $ update (const output) s res
    Stop -> []
    Goto i -> stepped $ s { pc = i }
    GotoZ v i -> stepped
       $ s { pc = if 0 == get (memory s) v then i
                  else succ $ pc s
           }

get_command s = do
  guard $ 0 <= pc s && pc s < length (program s)
  return $ program s !! pc s
       
stepped :: State -> [ State ]     
stepped s = return $ s { schritt = succ $ schritt s
		       , past    = s : past s
		       }

-- | wert einer variablen ändern
-- nächsten befehl anwählen
update :: (Integer -> Integer) -> State -> Register -> State
update fun s v = 
    let n = get (memory s) v
    in  s { memory = set ( memory s ) ( v, fun n )
          , pc = succ $ pc s
	  }

