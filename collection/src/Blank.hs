-- | Eine Aufgabe, bei der gar keine Studenten-Lösung akzeptiert wird
-- Tutor soll "von Hand" die Oks eintragen
-- und damit (schriftliche \/ mündliche) Seminar-Aufgaben bewerten 
-- Vorteil ist, daß die dann mit in der Auswertung auftauchen

{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DisambiguateRecordFields #-}

module Blank where

import Inter.Types
import Autolib.ToDoc
import Autolib.Reporter
import qualified Challenger as C
import Data.Typeable
import Autolib.ToDoc
import Autolib.Reader

data Blank = Blank deriving Typeable 

$(derives [makeReader, makeToDoc] [''Blank ])

instance Show Blank where show = render . toDoc

instance OrderScore Blank where
    scoringOrder _ = None

instance C.Partial Blank () () where
    report Blank () = inform $ vcat
	 [ text "Das ist eine schriftliche oder mündliche Aufgabe."
	 , text "Das autotool verwaltet lediglich die dabei erzielten Punkte."
	 , text "Diese werden von Ihrem Dozenten direkt in die Datenbank eingetragen."
	 , text "Es ist zwecklos, hier eine Lösung einzugeben."
	 ]
    initial Blank () = ()
    partial Blank () () = reject $ empty
    total Blank () () = reject $ empty

instance C.Measure Blank () () where
    measure _ _ _ = 1

make :: Make
make = direct Blank ()
