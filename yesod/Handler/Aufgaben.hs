module Handler.Aufgaben where

import Import
import Data.Set (Set, fromList, member)
import Data.Time.Clock (getCurrentTime)
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
  aktuelleZeit <- lift getCurrentTime
  stud <- requireAuthId
  Just (MNr mnr) <- liftM (fmap Student.mnr . listToMaybe) $ lift $ StudentDB.get_snr $ SNr stud
  istTutor <- runDB $ return . not . null =<< selectList [TutorStudentId ==. stud, TutorVorlesungId ==. vorlesung] []
  istEingeschrieben <- do
    einschreibungen <- lift $ VorlesungDB.snr_teilnehmer $ VNr $ keyToInt vorlesung
    return $ SNr stud `elem` einschreibungen
  aufgaben <- runDB $ selectList [AufgabeVorlesungId ==. vorlesung] []
  ergebnisse <- sequence $ do
    aufgabe' <- aufgaben
    let aufgabeId = entityKey aufgabe'
        aufgabe = entityVal aufgabe'
    return $ do
      einsendungen <- lift $ EinsendungDB.get_snr_anr (SNr stud) $ ANr $ keyToInt aufgabeId
      let okno = case einsendungen of
            [eins] ->  (Einsendung.ok eins, Einsendung.no eins, Einsendung.result eins)
            _ ->  (Oks 0, Nos 0 , Nothing)
      return (aufgabe, aufgabeId, okno)
  let ergebnisse' = filter (\(a,_,_) -> zeitStatus (aufgabeVon a) (aufgabeBis a) aktuelleZeit `member` disp) ergebnisse
      goal = sum $ do
        (aufgabe, _, _) <- ergebnisse
        guard $ aufgabeStatus aufgabe == Mandatory
        return 1
      done = sum $ do
        (aufgabe, _, (oks, _, _)) <- ergebnisse
        guard $ aufgabeStatus aufgabe == Mandatory
        guard $ oks > Oks 0
        return 1
      percent = (100 * done) `div` goal
  defaultLayout $
    $(widgetFile "aufgaben")
