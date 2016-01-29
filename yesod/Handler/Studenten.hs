module Handler.Studenten where

import Import
import Data.List (nub)

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
  let fromGNr (T.GNr gnr) = gnr
      zipClear list1 list2 = concat $ foldr (\(a, ma) b ->
          case ma of
            Nothing -> []
            Just a' -> [(a, a')]:b
        ) [] $ zip list1 list2
  einschreibungen <- liftIO $ VorlesungDB.snr_gnr_teilnehmer $ T.VNr $ keyToInt vorlesungId
  let gruppenIds = nub $ fmap (intToKey . fromGNr . snd) einschreibungen :: [GruppeId]
  gruppen <- liftM (zipClear gruppenIds) $ mapM (runDB . get) $ gruppenIds
  alleAufgaben <- runDB $ selectList [AufgabeVorlesungId ==. vorlesungId] []
  let aufgaben = do
        aufgabe <- alleAufgaben
        guard $ case auswahl of
          KeineAufgaben -> False
          PflichtAufgaben -> aufgabeStatus (entityVal aufgabe) == T.Mandatory
          AlleAufgaben -> True
        return aufgabe
  bewertungen <- liftIO $ liftM concat $ sequence $ do
    (snr, T.GNr gruppeId) <- einschreibungen
    return $ liftM maybeToList $ runMaybeT $ do
      mstudent <- lift $ StudentDB.get_snr snr
      student' <- MaybeT . return . listToMaybe $ mstudent
      einsendungen <- lift $ mapM (EinsendungDB.get_snr_anr snr . T.ANr . keyToInt . entityKey) aufgaben
      return $ BewertungEintrag student' gruppeId 0 $ fmap (maybe Nothing Einsendung.result . listToMaybe) einsendungen
  defaultLayout $
    $(widgetFile "studenten")
