{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DisambiguateRecordFields #-}

module FD.Top where

import FD.Data
import FD.Trace
import FD.Roll ( roll, config0, Config )

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Inter.Types hiding ( Var )
import Inter.Quiz
import Data.Typeable
import System.Random

data FD = FD deriving Typeable

derives [makeReader, makeToDoc] [''FD ]

instance Show FD where show = render . toDoc

instance OrderScore FD where 
    scoringOrder _ = None

instance Partial FD (Instance Int) [Step Int] where
    describe _ i  = vcat 
        [ text "Give a complete computation of an FD solver"
        , text "that determines satisfiability of:" </> toDoc (formula i)
        , text "in the structure:" </> toDoc (algebra i)
        , text "The computation must have these properties:" </> toDoc (modus i)
        ]
    initial _ i = steps0
    partial _ i steps = do
        FD.Trace.execute i steps
        return ()
    total _ i steps = do
        case max_solution_length $ modus i of
            Nothing -> return ()
            Just l -> when (length steps > l) $ reject 
                $ text "The number of computation steps" <+> parens (toDoc $ length steps)
                <+> text "is larger than" <+> toDoc l
        case reverse steps of
            Inconsistent : _ -> return ()
            Solved : _ -> return ()
            _ -> reject $ text "The computation must be complete (end with Consistent or Solved)"
                        
make_fixed = direct FD instance0

instance Generator FD Config ( Instance Int, [Step Int] ) where
    generator p conf key = FD.Roll.roll conf

instance Project FD ( Instance Int, [Step Int] ) (Instance Int) where
    project p (c, s) = c

make_quiz = quiz FD config0


