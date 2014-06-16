{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module DPLL.Roll where

import DPLL.Data
import DPLL.Trace ( Modus, modus0 )
import DPLL.Solve ( solve )

import Inter.Types hiding ( Var )

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Util.Perm

import Control.Applicative (( <$> ))
import Data.Function ( on )
import Data.List ( minimumBy )
import Data.Typeable
import System.Random

data Config =
     Config { modus :: Modus
            , solution_length_target :: Int
            , num_variables :: Int
            , num_literals_in_clause :: (Int,Int)
            , num_clauses :: Int
            , num_candidates :: Int
            }
    deriving Typeable

config0 = Config 
    { modus = modus0
    , num_variables = 4
    , solution_length_target = 15
    , num_literals_in_clause = (2, 4)
    , num_clauses = 10
    , num_candidates = 100
    }
    
derives [makeReader, makeToDoc] [ ''Config ]

instance Show Config where show = render . toDoc

roll conf = do
    cnfs <- forM [ 1 .. num_candidates conf ] $ \ k -> do
        cnf <- roll_cnf conf
        return ( cnf, solve (modus conf) cnf )
    let eval (c,s) = abs (length s - solution_length_target conf)
    return $ minimumBy ( compare `on` eval ) cnfs
   

roll_cnf conf = forM [ 1 .. num_clauses conf ] $ \ i -> do
    vs <- permIO $ map Variable [ 1 .. num_variables conf ]
    l <- randomRIO $ num_literals_in_clause conf
    forM (take l vs) $ \ v -> mkLiteral v <$> randomRIO (False,True)
    
pick xs = do
    i <-  randomRIO ( 0, length xs - 1) 
    return $ xs !! i
    
