{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module FD.Roll where

import FD.Data
import FD.Trace 
import FD.Solve ( solve, solveBestOf )

import Inter.Types hiding ( Var )

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Util.Perm

import Control.Applicative (( <$> ))
import Data.Function ( on )
import Data.List ( minimumBy, sort, nub )
import Data.Typeable
import System.Random
import qualified Data.Set as S
import qualified Data.Map as M

data Require = No | Yes { allow_extra :: Int }
    deriving Typeable

data Config =
     Config { modus :: Modus
            , solution_length_target :: Int
            , require_max_solution_length :: Require
            , universe_size :: Int
            , signature :: [ (Rel, Int) ]
            , variables :: [ Var ]
            , num_atoms :: Int
            , num_candidates :: Int
            , solve_best_of :: Int
            }
    deriving Typeable

config0 = Config 
    { FD.Roll.modus = modus0
    , solution_length_target = 15
    , require_max_solution_length = Yes { allow_extra = 1 }
    , universe_size = 5
    , signature = read "[(P, 1), (Q, 2), (R, 2)]"
    , FD.Roll.variables = read "[a, b, c, d]"
    , num_atoms = 4
    , num_candidates = 100
    , solve_best_of = 100
    }
    
derives [makeReader, makeToDoc] [ ''Config, ''Require ]

instance Show Config where show = render . toDoc

roll_algebra :: Config -> IO (Algebra Int)
roll_algebra conf = do
    let u = [ 1 .. universe_size conf ]
    rels <- forM (signature conf) $ \ (rel, ar) -> do
        let tuples = sequence $ replicate ar u
        sub <- random_subsequence tuples
        return (rel, S.fromList sub)
    return $ Algebra { universe = u, relations = M.fromList rels }

roll_formula conf = forM [ 1 .. num_atoms conf ] $ \ _ -> do
    (rel, ar) <- pick $ signature conf 
    args <- forM [ 1 .. ar ] $ \ _ -> pick $ FD.Roll.variables conf
    return $ Atom rel args
    
roll_instance conf = do
    form <- roll_formula conf
    alg <- roll_algebra conf
    let inst = Instance { algebra = alg, formula = form
                        , FD.Trace.modus = FD.Roll.modus conf }
    let s = solveBestOf ( solve_best_of conf ) inst
        l = length s
    return ( inst  
           { FD.Trace.modus = (FD.Roll.modus conf) {
                max_solution_length = case require_max_solution_length conf of
                    No -> Nothing
                    Yes { allow_extra = e } -> Just $ l + e
             } } , s )

roll conf = do
    candidates <- forM [ 1 .. num_candidates conf ] $ \ _ -> roll_instance conf
    let weight alg = sum $ map S.size $ M.elems $ relations alg
        eval (inst,sol) = 
            ( abs (length sol - solution_length_target conf)            
            , weight $ algebra inst -- prefer algebras that look smaller
            )
    return $ minimumBy ( compare `on` eval ) candidates


random_subsequence xs = do
    fs <- forM xs $ \ _ -> randomRIO (False,True)
    return $ map fst $ filter snd $ zip xs fs

{-


roll_cnf conf = nub <$> ( forM [ 1 .. num_clauses conf ] $ \ i -> do
    vs <- permIO $ map Variable [ 1 .. num_variables conf ]
    l <- randomRIO $ num_literals_in_clause conf
    forM (sort $ take l vs) $ \ v -> mkLiteral v <$> randomRIO (False,True)
  )    

-}

pick xs = do
    i <-  randomRIO ( 0, length xs - 1) 
    return $ xs !! i
    

