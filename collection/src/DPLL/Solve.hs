module DPLL.Solve where

import DPLL.Trace
import DPLL.Data

import qualified Control.Monad.Logic  as L

import Data.List ( partition, nub )
import Control.Monad ( guard )

handle :: Modus -> [Step] -> State -> L.Logic [Step ]
handle mo history a = 
    let ec =  evaluate_clause (assignment a) 
        (sat, unsat) = partition ( \ cl -> ec cl == Just True ) $ formula a
        (con, uncon) = partition ( \ cl -> ec cl == Just False ) $ unsat
        (unit, nunit) = partition (is_unit_clause a) $ uncon
    in  if null unsat then return $ SAT : history
        else if not $ null con then handle_conflicts mo history a con
        else if not $ null unit then handle_units mo history a unit
        else handle_decisions mo history a ( nub $ concat unit )

evaluate_clause b cl = 
    let vs = map (evaluate_literal b) cl
    in  if Just True `elem` vs then Just True
        else if Nothing `elem` vs then Nothing
        else Just False

handle_units mo history a (cl : nit) = 
    let vs = map (\ l -> (l, evaluate_literal (assignment a) l)) cl
        [ l ] = map fst $ filter ( (== Nothing) . snd ) vs
    in  handle mo (Propagate cl l : history) $ propagate a cl l

handle_decisions mo history a ls = interleaveMany $ do
    l <- ls
    guard $ negative l
    return $ handle mo (Decide l : history) $ decide a l
    
handle_conflicts mo history a con =
    if decision_level a <= 0 then return $ UNSAT : history
    else interleaveMany $ do
        cl <- con
        return $ handle mo (Backtrack :  Conflict cl : history) $ backtrack $ conflict a cl   

interleaveMany = foldr L.interleave ( fail undefined )
