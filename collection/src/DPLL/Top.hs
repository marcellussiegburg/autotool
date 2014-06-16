{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DisambiguateRecordFields #-}

module DPLL.Top where

import DPLL.Data
import DPLL.Trace
-- import DPLL.Pattern

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Inter.Types hiding ( Var )
import Inter.Quiz
import Data.Typeable
import System.Random

data DPLL = DPLL deriving Typeable

data Instance =
     Instance { modus :: Modus
              , cnf :: CNF              
              }
     deriving Typeable

instance0 :: Instance
instance0 = Instance 
    { modus = modus0
    , cnf = read "[[1,2,3],[-1,4],[2,-3],[1,-2,3,-4],[-2,3]]"
    }

derives [makeReader, makeToDoc] [''DPLL, ''Instance ]

instance Show DPLL where show = render . toDoc

instance OrderScore DPLL where 
    scoringOrder _ = None

instance Partial DPLL Instance [Step] where
    describe _ i  = vcat 
        [ text "Gesucht ist eine DPLL-Rechnung für die Formel" </> toDoc (cnf i)
        , text "mit diesen Eigenschaften" </> toDoc (modus i)
        ]
    initial _ i = 
        [ Decide (-2), Propagate [2,-3] (-3), SAT ]
    partial _ i steps = do
        DPLL.Trace.execute (modus i) (cnf i) steps
        return ()
    total _ i steps = do
        case reverse steps of
            SAT : _ -> return ()
            UNSAT : _ -> return ()
            _ -> reject $ text "die Rechnung soll mit SAT oder UNSAT enden"
                
        

make_fixed = direct DPLL instance0

