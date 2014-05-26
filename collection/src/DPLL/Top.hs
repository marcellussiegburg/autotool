{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DisambiguateRecordFields #-}

module DPLL.Top where

import DPLL.Data
import DPLL.Solve
import DPLL.Pattern

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Inter.Types hiding ( Var )
import Inter.Quiz
import Data.Typeable
import System.Random

data DPLL_Reconstruct = DPLL_Reconstruct deriving Typeable

data Instance =
     Instance { cnf :: [ Pattern [ Pattern Literal ] ]
              , steps :: [ Pattern Step ]
              }
     deriving Typeable

instance0 :: Instance
instance0 = Instance 
    { cnf = read "[[1,2,3],[-1,*],[*,-3],*,[-2,3]]"
    , steps = read "[ Decide -1 , * , Propagate    2 , Backtrack, Propagate -2, * , Success ]"
    }

derives [makeReader, makeToDoc] [''DPLL_Reconstruct, ''Instance ]

instance Show DPLL_Reconstruct where show = render . toDoc

instance OrderScore DPLL_Reconstruct where 
    scoringOrder _ = None

instance Partial DPLL_Reconstruct Instance CNF where
    describe _ i  = vcat 
        [ text "Gesucht ist eine Formel in CNF,"
        , text "die zu diesem Muster paßt:" </> toDoc (cnf i)
        , text "und deren DPLL-Schritte zu diesem Muster passen:" </> toDoc (steps i)
        ]
    initial _ i = do
        This cl <- cnf i
        return $ do This lit <- cl ; return lit
    partial _ i f = do
        matches (cnf i) f
    total _ i f = do
        let s = solve f
        inform $ text "Die DPLL-Rechnung ist" </> toDoc s
        matches (steps i) s

make_fixed = direct DPLL_Reconstruct instance0

