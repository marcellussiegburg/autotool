module CSP.Derive.Solve where

import CSP.Step
import CSP.Syntax
import CSP.Derive 
import Autolib.Reporter

import Data.Tree
import qualified Data.Set as S

cut k t @ ( Node f args ) = 
  Node f $ if k > 0 then ( map ( cut (k-1)) args ) else []

anno p = if null $ tau p then Just $ S.fromList $ map fst $ real p else Nothing
  

this =  Fix (Fix (Par [ 'c' ] (Pre 'c' Point)    
                             (Pre 'b' (Pre 'b' (Int (Pre 'a' Point) (Pre 'c' Point))))))


tree p = tree' ( Tau, p) 
tree' (s,p) = Node (s,p) $ map tree' $ successors p
