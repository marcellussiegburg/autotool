module DPLL.Solve where

import DPLL.Trace
import DPLL.Data

import qualified Control.Monad.Logic  as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( partition, nub )
import Data.Function ( on )
import Data.List ( minimumBy )
import Control.Monad ( guard )

solve :: Modus -> CNF -> [Step]
solve mo cnf = L.observe $ handle mo [] $ state0 cnf

solveBestOf :: Int -> Modus -> CNF -> [Step]
solveBestOf m mo cnf = 
    minimumBy (compare `on` length) $ L.observeMany m $ handle mo [] $ state0 cnf

handle :: Modus -> [Step] -> State -> L.Logic [Step ]
handle mo history a = 
    let ec =  evaluate_clause (assignment a) 
        (sat, unsat) = partition ( \ cl -> ec cl == Just True ) $ formula a
        (con, uncon) = partition ( \ cl -> ec cl == Just False ) $ unsat
        (unit, nunit) = partition (is_unit_clause a) $ uncon
        open = S.difference (variables a) ( M.keysSet $ assignment a )
    in  if null unsat then return $ reverse $ SAT : history
        else if not $ null con then handle_conflicts mo history a con
        else if not $ null unit then handle_units mo history a unit
        else handle_decisions mo history a ( S.toList  open )

evaluate_clause b cl = 
    let vs = map (evaluate_literal b) cl
    in  if Just True `elem` vs then Just True
        else if Nothing `elem` vs then Nothing
        else Just False

handle_units mo history a (cl : nit) = 
    let vs = map (\ l -> (l, evaluate_literal (assignment a) l)) cl
        [ l ] = map fst $ filter ( (== Nothing) . snd ) vs
    in  handle mo (Propagate cl l : history) $ propagate a cl l

handle_decisions mo history a vs = interleaveMany $ do
    v <- vs
    w <- case decisions_must_be_negative mo of
        True -> return False
        False -> L.interleave (return False) (return True)
    let l = mkLiteral v w
    return $ handle mo (Decide l : history) $ decide a l
    
handle_conflicts mo history a con = interleaveMany $ do
    cl <- con
    return $ handle_backtrack mo (Conflict cl : history) $ conflict a cl

handle_backtrack mo history a = 
    let dl = decision_level a
    in  if dl <= 0 then return $ reverse $ UNSAT : history
        else let a' = backtrack a
            in case conflict_clause a' of
                Nothing -> handle mo (Backtrack : history) a'
                Just cl -> error "should not happen" -- handle_backtrack mo (Backtrack : history) a'

interleaveMany = foldr L.interleave ( fail undefined )
