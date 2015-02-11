module Handler.AufgabeEinstellungen where

import Import

import qualified Control.Aufgabe.Typ as A
import qualified Control.Types as T

data AufgabeFormDaten = AufgabeFormDaten {
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

highscores ::  [(AutotoolMessage, Maybe Highscore)]
highscores = [(MsgHighscoreKeine, Nothing), (MsgHighscoreHoch, Just Hoch), (MsgHighscoreNiedrig, Just Niedrig)]

stati :: [(AutotoolMessage, Status)]
stati = [(MsgStatusDemonstration, Demonstration), (MsgStatusPflicht, Pflicht), (MsgStatusOptional, Optional)]

aufgabeForm :: Maybe AufgabeFormDaten -> AForm Handler AufgabeFormDaten
aufgabeForm maufgabe = do
  AufgabeFormDaten
    <$> aopt textField (bfs MsgAufgabeHinweis) (fmap hinweis maufgabe)
    <*> areq (selectFieldList highscores) (bfs MsgAufgabeHighscore) (fmap highscore maufgabe)
    <*> areq (selectFieldList stati) (bfs MsgAufgabeStatus) (fmap status maufgabe)
    <*> areq (jqueryDayField def) (bfsFormControl MsgAufgabeBeginnDatum) (fmap beginn maufgabe)
    <*> areq timeField (bfs MsgAufgabeBeginnZeit) (fmap beginnZeit maufgabe)
    <*> areq (jqueryDayField def) (bfsFormControl MsgAufgabeEndeDatum) (fmap ende maufgabe)
    <*> areq timeField (bfs MsgAufgabeEndeZeit) (fmap endeZeit maufgabe)
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeEinstellen "btn-success" [])
