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
         in  if null empties 
             then L.interleave ( propagate inst history a )
                  ( decide inst history a )
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
    
propagate inst history a = interleaveMany $ do
    let fvs = free_vars a $ formula inst
    vs <- subsets fvs
    guard $ not $ S.null vs
    guard $ case allow_hyperarc_propagation_up_to $ modus inst of
        Nothing -> True
        Just bound -> S.size vs <= bound
    let ats = filter ( \ at -> S.isSubsetOf (free_vars a [at]) vs ) 
            $ formula inst
    var <- S.toList $ variables ats
    let dom = current a M.! var
    guard $ length dom >= 2
    let range = do
            elt <- dom
            guard $ not $ null $ satisfying_assignments inst a ats var elt
            return elt
    guard $ range /= dom
    return $ apply inst history a $ Arc_Consistency_Deduction 
                   { atoms = ats, variable = var, restrict_to = range }

interleaveMany = foldr L.interleave ( fail undefined )

subsets s = case S.minView s of
    Nothing -> [s]
    Just (x,t) -> 
        let ss = subsets t
        in  ss ++ map (S.insert x) ss
