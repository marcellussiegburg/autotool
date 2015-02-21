module Handler.Resultate where

import Import
import Handler.Studenten (Aufgaben (AlleAufgaben), studentenListe)

getResultateR :: VorlesungId -> Handler Html
getResultateR = studentenListe AlleAufgaben
