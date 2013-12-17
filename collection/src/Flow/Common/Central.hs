{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable  #-}

module Flow.Common.Central where

import Flow.Trace
import Flow.Conditions
import Flow.Actions
import Flow.State

import Flow.Common.Data 
import Flow.Common.Shape
import Flow.Common.Semantics

import Challenger.Partial
import Inter.Types

import Autolib.NFA.Subseteq
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Informed
import Autolib.Reporter.Set (subeq, eq)

import Data.Typeable

import Data.Set ( Set )
import qualified Data.Set 


data Control_Flow = Control_Flow
    deriving ( Read, Show, Typeable )

instance OrderScore Control_Flow where
    scoringOrder _ = Increasing

instance Partial Control_Flow ( Statement, Condition ) Statement where

    describe p (i, c) = vcat
        [ text "Gesucht ist ein Programm,"
	, text "das äquivalent ist zu" </>  toDoc i
        , text "und diese Bedingungen erfüllt" </> toDoc c
	]

    initial p (i, c) = example
   
    partial p (i, c) b = do
        Flow.Common.Shape.check c b
        partializer i b

    total p (i, c) b = do
        let s = all_states
              $ Data.Set.union (conditions i) 
                               (conditions b)
        totalizer ( semantics s i ) 
                  ( semantics s b )

control_flow_fixed :: Make
control_flow_fixed = 
    direct Control_Flow (example, And [ Flat, No_Loops, Simple_Branches ] )

-------------------------------------------------------

partializer i b = do
    comparing "Zustands-Prädikate" (conditions i) (conditions b)
    comparing "elementare Anweisungen" (actions i) (actions b)

comparing tag pi pb = do
    when ( pi /= pb ) $ inform $ vcat 
           [ text "Hinweis: vorkommende" <+> text tag
           , nest 4 $ text "in Aufgabenstellung" </> toDoc pi
           , nest 4 $ text "in Einsendung" </> toDoc pb
           , text "stimmen nicht überein."
           ]


totalizer orig0 this0 = do 
    orig <- orig0
    let o = informed ( text "Spursprache des Programms aus Aufgabenstellung" ) orig
    this <- this0
    let t = informed ( text "Spursprache des Programms aus Ihrer Einsendung" ) this
    ok1 <- subsetequ o t
    ok2 <- subsetequ t o
    assert ( ok1 && ok2 ) $ text "Spursprachen stimmen überein?"


