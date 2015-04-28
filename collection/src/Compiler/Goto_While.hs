module Compiler.Goto_While where

import qualified While as W
import qualified Goto as G

compile :: G.Program -> W.Program
compile p =
  let c = G.free_register p ; h = c+1
  in  W.Seq (W.Inc h) $ W.While h $ body c h $ zip [0..] p

body c h ips = case ips of
  [] -> W.Skip
  (i,s) : rest ->
      W.IfZ c (statement c h (i,s))
    $ W.Seq (W.Dec c) $ body c h rest
    
statement c h (i,s) = case s of
  G.Inc r -> W.Seq (W.Inc r) $ assign c (i+1)
  G.Dec r -> W.Seq (W.Dec r) $ assign c (i+1)
  G.Stop  -> W.Dec h
  G.Goto l -> assign c l
  G.GotoZ r l -> W.IfZ r (assign c l) (assign c (i+1))

assign c i = foldr W.Seq W.Skip $ replicate i $ W.Inc c
