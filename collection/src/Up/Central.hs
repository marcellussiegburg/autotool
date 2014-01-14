{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language MultiParamTypeClasses #-}

module Up.Central where

import Up.Type
import Up.Shape
import Up.Exec
import Up.Iso

import Inter.Types
import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter
import Autolib.Informed
import Autolib.Size
import Autolib.Reader
import Autolib.ToDoc

data Up = Up
    deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Up where
    scoringOrder _ = Increasing

data Config = Config { links :: Links
                     , pattern :: Block
                     , max_steps :: Int
                     }
    deriving Typeable

derives [makeReader, makeToDoc] [''Config]

instance C.Measure Up Config Block where
    measure p c b = 
        fromIntegral $ size b - size (pattern c)

instance C.Verify Up Config where
    verify p c = return ()

instance C.Partial Up Config Block where
    report p c = do
        inform $ vcat 
            [ text "Ersetzen Sie im Programm"
            , nest 4 $ toDoc $ pattern c
            , text "jedes 'missing' durch eine Deklaration oder einen Ausdruck,"
            , text "so daß nach höchstens" <+> toDoc (max_steps c) <+> text "Auswertungsschritten"
            , text "die Anweisung 'halt' erreicht wird"
            , text "und die Frames dann folgende Verweise enthalten:"
            , nest 4 $ nicely $ links c
            ]
    initial p c = pattern c

    partial p c b = do
        inform $ text "Stimmt die Einsendung syntaktisch mit dem Muster überein?"
        match (pattern c) b
        inform $ text "Ja."
  
    total p c b = do
        inform $ text "Stimmt die Einsendung semantisch mit der Vorgabe überein?"
        s <- Up.Exec.execute (max_steps c) b
        inform $ text "Dieser Speicherzustand wird erreicht:"
        inform $ nest 4 $ toDoc s
        iso (links c) s

make = direct Up config0

config0 = Config 
    { links = Links [(0,0),(1,1),(2,1)]
    , pattern = read "{ f = function () { halt; } ; g = function (y) { f (); } ; missing; }"
    , max_steps = 10
    }

  
