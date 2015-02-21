module Handler.ResultatePflicht where

import Import 
import Handler.Studenten (Aufgaben(PflichtAufgaben), studentenListe)

getResultatePflichtR :: VorlesungId -> Handler Html
getResultatePflichtR = studentenListe PflichtAufgaben
