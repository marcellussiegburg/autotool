{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DisambiguateRecordFields #-}

module DPLL.Top where

import DPLL.Data
import DPLL.Trace
-- import DPLL.Pattern
import DPLL.Roll 

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
              , max_solution_length :: Maybe Int
              }
     deriving Typeable

instance0 :: Instance
instance0 = Instance 
    { modus = modus0
    , cnf = read "[[1,2,3],[-1,4],[2,-3],[1,-2,3,-4],[-2,3]]"
    , max_solution_length = Nothing
    }

derives [makeReader, makeToDoc] [''DPLL, ''Instance ]

instance Show DPLL where show = render . toDoc

instance OrderScore DPLL where 
    scoringOrder _ = None

instance Partial DPLL Instance [Step] where
    describe _ i  = vcat 
        [ text "Gesucht ist eine vollständige DPLL-Rechnung" </>
          case max_solution_length i of
            Nothing -> empty
            Just l -> text "mit höchstens" <+> toDoc l <+> text "Schritten"
        , text "mit diesen Eigenschaften:" </> toDoc (DPLL.Top.modus i)
        , text "für diese Formel:" </> toDoc (cnf i)
        ]
    initial _ i = 
        [ Decide (-2), Propagate [2,-3] (-3), SAT ]
    partial _ i steps = do
        DPLL.Trace.execute (DPLL.Top.modus i) (cnf i) steps
        return ()
    total _ i steps = do
        case max_solution_length i of
            Nothing -> return ()
            Just l -> when (length steps > l) $ reject 
                $ text "Die Anzahl der Schritte" <+> parens (toDoc $ length steps)
                <+> text "ist größer als" <+> toDoc l
        case reverse steps of
            SAT : _ -> return ()
            UNSAT : _ -> return ()
            _ -> reject $ text "die Rechnung soll vollständig sein (mit SAT oder UNSAT enden)"
                        
make_fixed = direct DPLL instance0

instance Generator DPLL Config ( Instance, [Step] ) where
    generator p conf key = do
        (c, s) <- roll conf
        return ( Instance { modus = DPLL.Roll.modus conf
                          , max_solution_length = case require_max_solution_length conf of
                                DPLL.Roll.No -> Nothing
                                Yes { allow_extra = e } -> Just $ length s + e 
                          , cnf = c 
                          } , s )
instance Project DPLL ( Instance, [Step] ) Instance where
    project p (c, s) = c

make_quiz = quiz DPLL config0
