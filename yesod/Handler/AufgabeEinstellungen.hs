module Handler.AufgabeEinstellungen where

import Import hiding (delete)
import Handler.AufgabeVorlagen (getVorlagen)

import Data.List (delete)

import qualified Control.Aufgabe.Typ as A
import qualified Control.Types as T

data AufgabeFormDaten = AufgabeFormDaten {
    name :: Text,
    hinweis :: Maybe Text,
    highscore :: Maybe Highscore,
    status :: Status,
    beginn :: Day,
    beginnZeit :: TimeOfDay,
    ende :: Day,
    endeZeit :: TimeOfDay
} deriving (Show, Read)

instance PathPiece AufgabeFormDaten where
  fromPathPiece t = case reads $ unpack t of
      [(i, "")] -> Just i
      _ -> Nothing
  toPathPiece = pack . show

aufgabeToFormDaten :: A.Aufgabe -> AufgabeFormDaten
aufgabeToFormDaten aufgabe =
  AufgabeFormDaten {
    name = pack . T.toString . A.name $ aufgabe,
    hinweis = Just . pack . T.toString . A.remark $ aufgabe,
    highscore = hiLoToFormDaten $ A.highscore aufgabe,
    status = statusToFormDaten $ A.status aufgabe,
    beginn = utctDay $ timeToUTCTime $ A.von aufgabe,
    beginnZeit = timeToTimeOfDay $ utctDayTime $ timeToUTCTime $ A.von aufgabe,
    ende = utctDay $ timeToUTCTime $ A.bis aufgabe,
    endeZeit = timeToTimeOfDay $ utctDayTime $ timeToUTCTime $ A.bis aufgabe
  }

data Highscore = Hoch | Niedrig deriving (Eq, Show, Read)

instance PathPiece Highscore where
  fromPathPiece t = case reads $ unpack t of
    [(i, "")] -> Just i
    _ -> Nothing
  toPathPiece = pack . show

hiLoToFormDaten :: T.HiLo -> Maybe Highscore
hiLoToFormDaten h = case h of
  T.High -> Just Hoch
  T.Low -> Just Niedrig
  T.Keine -> Nothing

formDatenToHiLo :: Maybe Highscore -> T.HiLo
formDatenToHiLo h = case h of
  Just Hoch -> T.High
  Just Niedrig -> T.Low
  Nothing -> T.Keine

data Status = Demonstration | Optional | Pflicht deriving (Eq, Show, Read)

instance PathPiece Status where
  fromPathPiece t = case reads $ unpack t of
    [(i, "")] -> Just i
    _ -> Nothing
  toPathPiece = pack . show

statusToFormDaten :: T.Status -> Status
statusToFormDaten s = case s of
  T.Mandatory -> Pflicht
  T.Optional -> Optional
  T.Demo -> Demonstration

formDatenToStatus :: Status -> T.Status
formDatenToStatus s = case s of
  Pflicht -> T.Mandatory
  Optional -> T.Optional
  Demonstration -> T.Demo

highscores ::  [(AutotoolMessage, Maybe Highscore)]
highscores = [(MsgHighscoreKeine, Nothing), (MsgHighscoreHoch, Just Hoch), (MsgHighscoreNiedrig, Just Niedrig)]

stati :: [(AutotoolMessage, Status)]
stati = [(MsgStatusDemonstration, Demonstration), (MsgStatusPflicht, Pflicht), (MsgStatusOptional, Optional)]

aufgabeForm :: AufgabeTyp -> Maybe AufgabeFormDaten -> AForm Handler AufgabeFormDaten
aufgabeForm aufgabeTyp maufgabe = AufgabeFormDaten
    <$> areq nameField (bfs MsgAufgabeName) (fmap name maufgabe)
    <*> (fmap unTextarea <$> aopt textareaField (bfs MsgAufgabeHinweis) (fmap Textarea . hinweis <$> maufgabe))
    <*> areq (selectFieldList highscores) (bfs MsgAufgabeHighscore) (fmap highscore maufgabe)
    <*> areq (selectFieldList stati) (bfs MsgAufgabeStatus) (fmap status maufgabe)
    <*> areq (jqueryDayField def) (bfsFormControl MsgAufgabeBeginnDatum) (fmap beginn maufgabe)
    <*> areq timeField (bfs MsgAufgabeBeginnZeit) (fmap beginnZeit maufgabe)
    <*> areq (jqueryDayField def) (bfsFormControl MsgAufgabeEndeDatum) (fmap ende maufgabe)
    <*> areq timeField (bfs MsgAufgabeEndeZeit) (fmap endeZeit maufgabe)
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeEinstellen "btn-success" [])
  where
    nameField = flip checkM textField $ \ n -> do
      vorlagen <- getVorlagen aufgabeTyp
      let vorlagen' = maybe vorlagen ((flip delete vorlagen) . name) maufgabe
      if n `elem` vorlagen'
        then return $ Left MsgAufgabeNameVergeben
        else return $ Right n
