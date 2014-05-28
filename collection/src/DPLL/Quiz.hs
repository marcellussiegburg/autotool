{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module DPLL.Quiz where

import DPLL.Pattern

import Inter.Types hiding ( Var )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Util.Perm

import Data.Function ( on )
import Data.List ( minimumBy )
import Data.Typeable
import System.Random

data Config =
     Config { num_variables :: Int
            , num_literals_in_clause :: (Int,Int)
            , num_clauses :: Int
            , num_candidates :: Int
            , num_holes_in_formula :: Int
            , num_holes_in_trace :: Int
            }
    deriving Typeable

config0 = Config 
    { num_variables = 4
    , num_literals_in_clause = (2, 4)
    , num_clauses = 10
    , num_candidates = 10
    , num_holes_in_formula = 8
    , num_holes_in_trace = 8
    }
    
derives [makeReader, makeToDoc] [ ''Config ]

roll_cnf conf = forM [ 1 .. num_clauses conf ] $ \ i -> do
    vs <- permIO [ 1 .. num_variables conf ]
    l <- randomRIO $ num_literals_in_clause conf
    forM (take l vs) $ \ v -> pick [ v, negate v ]
    
punch_cnf conf cnf = 
    punches (num_holes_in_formula conf) cnf

pick xs = do
    i <-  randomRIO ( 0, length xs - 1) 
    return $ xs !! i
    
