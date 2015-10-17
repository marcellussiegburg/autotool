module Handler.Studenten where

import Import
import Data.List (nub)

import qualified Control.Aufgabe.DB as AufgabeDB
import qualified Control.Aufgabe.Typ as Aufgabe
import qualified Control.Gruppe.DB as GruppeDB
import qualified Control.Gruppe.Typ as Gruppe
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import qualified Control.Stud_Aufg.DB as EinsendungDB
import qualified Control.Stud_Aufg.Typ as Einsendung
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Types as T

data BewertungListe = BewertungEintrag {
  student :: Student.Student,
  gruppe :: Int,
  punkte :: Int,
  ergebnisse :: [Maybe T.Wert]
}

data Aufgaben =
  KeineAufgaben |
  PflichtAufgaben |
  AlleAufgaben deriving Eq

titel :: Aufgaben -> AutotoolMessage
titel aufgaben = case aufgaben of
  KeineAufgaben -> MsgStudenten
  PflichtAufgaben -> MsgResultatePflicht
  AlleAufgaben -> MsgAlleResultate

link :: Aufgaben -> (VorlesungId -> Route Autotool)
link aufgaben = case aufgaben of
  KeineAufgaben -> StudentenR
  PflichtAufgaben -> ResultatePflichtR
  AlleAufgaben -> ResultateR

davor :: Aufgaben -> Aufgaben
davor aufgaben = case aufgaben of
  KeineAufgaben -> AlleAufgaben
  PflichtAufgaben -> KeineAufgaben
  AlleAufgaben -> PflichtAufgaben

danach :: Aufgaben -> Aufgaben
danach = davor . davor

getStudentenR :: VorlesungId -> Handler Html
getStudentenR = studentenListe KeineAufgaben

studentenListe :: Aufgaben -> VorlesungId -> Handler Html
studentenListe auswahl vorlesungId = do
  einschreibungen <- liftIO $ VorlesungDB.snr_gnr_teilnehmer $ T.VNr $ keyToInt vorlesungId
  gruppen <- liftIO $ liftM concat $ mapM GruppeDB.get_gnr $ nub $ fmap snd einschreibungen
  alleAufgaben <- liftIO $ AufgabeDB.get $ Just $ T.VNr $ keyToInt vorlesungId
  let aufgaben = do
        aufgabe <- alleAufgaben
        guard $ case auswahl of
          KeineAufgaben -> False
          PflichtAufgaben -> Aufgabe.status aufgabe == T.Mandatory
          AlleAufgaben -> True
        return aufgabe
  bewertungen <- liftIO $ liftM concat $ sequence $ do
    (snr, T.GNr gruppeId) <- einschreibungen
    return $ liftM maybeToList $ runMaybeT $ do
      mstudent <- lift $ StudentDB.get_snr snr
      student' <- MaybeT . return . listToMaybe $ mstudent
      einsendungen <- lift $ mapM (EinsendungDB.get_snr_anr snr . Aufgabe.anr) aufgaben
      return $ BewertungEintrag student' gruppeId 0 $ fmap (maybe Nothing Einsendung.result . listToMaybe) einsendungen
  let fromGNr gnr = let T.GNr g = gnr in g
  defaultLayout $ do
    $(widgetFile "studenten")
