module FD.Solve where

import FD.Trace
import FD.Data

import qualified Autolib.Reporter
import Autolib.ToDoc

import qualified Control.Monad.Logic  as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List ( partition, nub )
import Data.Function ( on )
import Data.List ( minimumBy )
import Control.Monad ( guard )

solve :: (ToDoc u, Ord u) 
      => Instance u -> [Step u]
solve inst = L.observe $ handle inst [] $ state0 inst

solveBestOf :: (ToDoc u, Ord u)
            => Int -> Instance u -> [ Step u ]
solveBestOf m inst = 
    minimumBy (compare `on` length) $ L.observeMany m $ handle inst [] $ state0 inst

handle :: (ToDoc u, Ord u)
       => Instance u -> [ Step u ] -> State u -> L.Logic [ Step u ]
handle inst history a = 
    if is_solved inst a 
    then return $ reverse $ Solved : history
    else let empties = M.toList $ M.filter null $ current a 
         in  if null empties then propagate inst history a
             else handle_conflict inst history a

apply inst history a step = 
    let a' = case Autolib.Reporter.result $ work inst a step of
            Just a' -> a'
            Nothing -> error $ render $ vcat [ toDoc a, toDoc history, toDoc step ]
    in  handle inst (step : history) a'

handle_conflict inst history a = 
    if at_top a 
    then return $ reverse $ Inconsistent : history
    else apply inst history a Backtrack

decide inst history a = interleaveMany $ do
    (var, dom) <- M.toList $ current a
    guard $ free a var
    elt <- case decisions_must_be_increasing $ modus inst of
        False -> dom
        True -> [ minimum dom ]
    return $ apply inst history a $ Decide var elt
    
propagate inst history a = 
    let props = do
            at <- formula inst
            let fvs = free_vars a [at]
            guard $ case allow_hyperarc_propagation_up_to $ modus inst of
                Nothing -> True
                Just bound -> S.size fvs <= bound
            var <- S.toList $ variables [at]
            let dom = current a M.! var
            let range = do
                    elt <- dom
                    guard $ not $ null $ satisfying_assignments inst a [at] var elt
                    return elt
            guard $ range /= dom
            return $ Arc_Consistency_Deduction 
                   { atom = at, variable = var, restrict_to = range }
    in  if null props 
        then decide inst history a
        else interleaveMany $ do 
            step <- props 
            return $ apply inst history a step

interleaveMany = foldr L.interleave ( fail undefined )
