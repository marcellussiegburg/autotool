module Handler.Studenten where

import Import
import Data.List (nub)

import qualified Control.Gruppe.DB as GruppeDB
import qualified Control.Gruppe.Typ as Gruppe
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Types as T

data BewertungListe = BewertungEintrag {
  student :: Student.Student,
  gruppe :: Int,
  punkte :: Int,
  ergebnisse :: [Ergebnis]
}

type Ergebnis = Maybe (Int, Int)
data Aufgaben = KeineAufgaben | PflichtAufgaben | AlleAufgaben

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

studentenListe :: Aufgaben -> Int -> Handler Html
studentenListe auswahl vorlesung = do
  einschreibungen <- liftIO $ VorlesungDB.snr_gnr_teilnehmer $ T.VNr vorlesung
  gruppen <- liftIO $ liftM concat $ mapM GruppeDB.get_gnr $ nub $ fmap snd einschreibungen
  bewertungen <- liftIO $ liftM concat $ sequence $ do
    (snr, T.GNr gruppeId) <- einschreibungen
    return $ liftM maybeToList $ runMaybeT $ do
      mstudent <- lift $ StudentDB.get_snr snr
      student' <- MaybeT . return . listToMaybe $ mstudent
      return $ BewertungEintrag student' gruppeId 0 []
  let fromGNr gnr = let T.GNr g = gnr in g
      aufgaben = case auswahl of
        KeineAufgaben -> []
        PflichtAufgaben -> ["peano-fold-times"] :: [Text]
        AlleAufgaben -> ["guess-the-number", "multiply-peano-numbers", "peano-fold-times"] :: [Text]
  defaultLayout $ do
    $(widgetFile "studenten")
