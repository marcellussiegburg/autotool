{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DisambiguateRecordFields #-}

module FD.Top where

import FD.Data
import FD.Trace
-- import FD.Roll 

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Inter.Types hiding ( Var )
import Inter.Quiz
import Data.Typeable
import System.Random

data FD = FD deriving Typeable

instance0 :: Instance Int
instance0 = Instance 
    { modus = modus0
    , algebra = algebra0
    , formula = formula0
    }

derives [makeReader, makeToDoc] [''FD ]

instance Show FD where show = render . toDoc

instance OrderScore FD where 
    scoringOrder _ = None

instance Partial FD (Instance Int) [Step Int] where
    describe _ i  = vcat 
        [ text "Gesucht ist eine vollständige FD-Rechnung" 
        , text "mit diesen Eigenschaften:" </> toDoc (modus i)
        , text "für diese Formel:" </> toDoc (formula i)
        , text "in dieser Struktur:" </> toDoc (algebra i)
        ]
    initial _ i = steps0
    partial _ i steps = do
        FD.Trace.execute i steps
        return ()
    total _ i steps = do
        case max_solution_length $ modus i of
            Nothing -> return ()
            Just l -> when (length steps > l) $ reject 
                $ text "Die Anzahl der Schritte" <+> parens (toDoc $ length steps)
                <+> text "ist größer als" <+> toDoc l
        case reverse steps of
            Inconsistent : _ -> return ()
            Solved : _ -> return ()
            _ -> reject $ text "die Rechnung soll vollständig sein (mit Consistent oder Solved enden)"
                        
make_fixed = direct FD instance0

{-

instance Generator FD Config ( Instance, [Step] ) where
    generator p conf key = do
        (c, s, o) <- roll conf
        return ( Instance { modus = FD.Roll.modus conf
                          , max_solution_length = case require_max_solution_length conf of
                                FD.Roll.No -> Nothing
                                Yes { allow_extra = e } -> Just $ o + e 
                          , cnf = c 
                          } , s )
instance Project FD ( Instance, [Step] ) Instance where
    project p (c, s) = c

make_quiz = quiz FD config0

-}
