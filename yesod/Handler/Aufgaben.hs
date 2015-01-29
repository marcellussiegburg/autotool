module Handler.Aufgaben where

import Import
import Data.Set (Set, fromList, member)
import qualified Control.Aufgabe.DB as AufgabeDB
import qualified Control.Aufgabe.Typ as Aufgabe
import qualified Control.Stud_Aufg.DB as EinsendungDB
import qualified Control.Stud_Aufg.Typ as Einsendung
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Vorlesung.Typ as Vorlesung
import Control.Types

getAufgabenR :: VorlesungId -> Handler Html
getAufgabenR = aufgabenListe MsgAufgabenAlle $ fromList [Current, Early, Late]

aufgabenListe :: AutotoolMessage -> Set TimeStatus -> VorlesungId -> Handler Html
aufgabenListe titel disp vorlesung = do
  let getAufgabe a = let ANr anr = Aufgabe.anr a
                     in anr
  stud <- requireAuthId
  istTutor <- do
    vorlesungen <- lift $ VorlesungDB.get_tutored $ SNr stud
    return $ VNr vorlesung `elem` map Vorlesung.vnr vorlesungen
  istEingeschrieben <- do
    einschreibungen <- lift $ VorlesungDB.snr_teilnehmer $ VNr vorlesung
    return $ SNr stud `elem` einschreibungen
  aufgaben <- lift $ AufgabeDB.get $ Just $ VNr vorlesung
  let aufgaben' = filter (\a -> Aufgabe.timeStatus a `member` disp) aufgaben
  ergebnisse <- sequence $ do
    aufgabe <- aufgaben'
    return $ do
      einsendungen <- lift $ EinsendungDB.get_snr_anr (SNr stud) (Aufgabe.anr aufgabe)
      let okno = case einsendungen of
            [eins] ->  (Einsendung.ok eins, Einsendung.no eins, Einsendung.result eins)
            _ ->  (Oks 0, Nos 0 , Nothing)
      return (aufgabe, okno)
  defaultLayout $ do
    $(widgetFile "aufgaben")
