module Handler.Aufgaben where

import Import
import Data.Set (Set, fromList, member)
import qualified Control.Aufgabe.DB as AufgabeDB
import qualified Control.Aufgabe.Typ as Aufgabe
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import qualified Control.Stud_Aufg.DB as EinsendungDB
import qualified Control.Stud_Aufg.Typ as Einsendung
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Vorlesung.Typ as Vorlesung
import Control.Types

getAufgabenR :: VorlesungId -> Handler Html
getAufgabenR = aufgabenListe $ fromList [Current, Early, Late]

aufgabenListe :: Set TimeStatus -> VorlesungId -> Handler Html
aufgabenListe disp vorlesung = do
  let getAufgabe a = let ANr anr = Aufgabe.anr a
                     in anr
  stud <- requireAuthId
  Just (MNr mnr) <- liftM (fmap Student.mnr . listToMaybe) $ lift $ StudentDB.get_unr $ UNr stud
  istTutor <- do
    vorlesungen <- lift $ VorlesungDB.get_tutored $ SNr stud
    return $ VNr (keyToInt vorlesung) `elem` map Vorlesung.vnr vorlesungen
  istEingeschrieben <- do
    einschreibungen <- lift $ VorlesungDB.snr_teilnehmer $ VNr $ keyToInt vorlesung
    return $ SNr stud `elem` einschreibungen
  aufgaben <- lift $ AufgabeDB.get $ Just $ VNr $ keyToInt vorlesung
  ergebnisse <- sequence $ do
    aufgabe <- aufgaben
    return $ do
      einsendungen <- lift $ EinsendungDB.get_snr_anr (SNr stud) (Aufgabe.anr aufgabe)
      let okno = case einsendungen of
            [eins] ->  (Einsendung.ok eins, Einsendung.no eins, Einsendung.result eins)
            _ ->  (Oks 0, Nos 0 , Nothing)
      return (aufgabe, okno)
  let ergebnisse' = filter (\(a,_) -> Aufgabe.timeStatus a `member` disp) ergebnisse
      goal = sum $ do
        (aufgabe, _) <- ergebnisse
        guard $ Aufgabe.status aufgabe == Mandatory
        return 1
      done = sum $ do
        (aufgabe, (oks, _, _)) <- ergebnisse
        guard $ Aufgabe.status aufgabe == Mandatory
        guard $ oks > Oks 0
        return 1
      percent = (100 * done) `div` goal
  defaultLayout $ do
    $(widgetFile "aufgaben")
