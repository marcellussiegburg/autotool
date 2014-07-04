{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DisambiguateRecordFields #-}

module DPLLT.Top where

import DPLLT.Data
import DPLLT.Trace
-- import DPLL.Pattern
-- import DPLL.Roll 

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Inter.Types hiding ( Var )
import Inter.Quiz
import Data.Typeable
import System.Random

data DPLLT = DPLLT deriving Typeable

data Instance =
     Instance { modus :: Modus
              , cnf :: CNF         
              , max_solution_length :: Maybe Int
              }
     deriving Typeable

instance0 :: Instance
instance0 = Instance 
    { modus = modus0
    , cnf = read "[[p,q,r],[!p, 0 <= x+y ],[q,!r],[p,!q,r, 0 <= -x],[!q,r]]"
    , max_solution_length = Nothing
    }

derives [makeReader, makeToDoc] [''DPLLT, ''Instance ]

instance Show DPLLT where show = render . toDoc

instance OrderScore DPLLT where 
    scoringOrder _ = None

instance Partial DPLLT Instance [Step] where
    describe _ i  = vcat 
        [ text "Gesucht ist eine vollständige DPLLT-Rechnung" </>
          case max_solution_length i of
            Nothing -> empty
            Just l -> text "mit höchstens" <+> toDoc l <+> text "Schritten"
        , text "mit diesen Eigenschaften:" </> toDoc (DPLLT.Top.modus i)
        , text "für diese Formel:" </> toDoc (cnf i)
        ]
    initial _ i = 
        let p = case clause_learning $ DPLLT.Top.modus i of
                True -> read "Backjump 1 [ p, ! q ]"
                False -> Backtrack 
        in  read "[ Decide ! p, Propagate [p, !q] !q, Conflict [r, !s], p, SAT ]"
    partial _ i steps = do
        DPLLT.Trace.execute (DPLLT.Top.modus i) (cnf i) steps
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
                        
make_fixed = direct DPLLT instance0

{-

instance Generator DPLLT Config ( Instance, [Step] ) where
    generator p conf key = do
        (c, s, o) <- roll conf
        return ( Instance { modus = DPLLT.Roll.modus conf
                          , max_solution_length = case require_max_solution_length conf of
                                DPLLTT.Roll.No -> Nothing
                                Yes { allow_extra = e } -> Just $ o + e 
                          , cnf = c 
                          } , s )
instance Project DPLLT ( Instance, [Step] ) Instance where
    project p (c, s) = c

make_quiz = quiz DPLLT config0

-}
