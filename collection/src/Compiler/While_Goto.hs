module Compiler.While_Goto where

import qualified While as W
import qualified Goto as G

compile :: W.Program -> G.Program
compile p = let (e,q) = comp (0,p) in  q ++ [ G.Stop ]

comp :: (Int, W.Program) -> (Int, G.Program)
comp (a,p) = case p of
  W.Skip -> (a, []) 
  W.Inc r -> (a+1, [G.Inc r])
  W.Dec r -> (a+1, [G.Dec r])
  W.Seq p1 p2 ->
    let (m,q1) = comp (a, p1); (e,q2) = comp (m, p2)
    in  (e,q1 ++ q2)
  W.IfZ r p1 p2 ->
    let (h,q2) = comp (a+1,p2) ; (e,q1) = comp (h+1,p1)
    in (e, [G.GotoZ r (h+1)] ++ q2 ++ [G.Goto e] ++ q1)
  W.While r p ->
    let (e,q) = comp (a+1,p)
    in  (e+1, [ G.GotoZ r (e+1)] ++ q ++ [G.Goto a])
          
