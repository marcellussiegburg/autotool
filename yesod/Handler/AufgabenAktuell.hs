module Handler.AufgabenAktuell where

import Import
import Data.Set (fromList)
import Control.Types

import Handler.Aufgaben (aufgabenListe)

getAufgabenAktuellR :: VorlesungId -> Handler Html
getAufgabenAktuellR = aufgabenListe $ fromList [Current]
