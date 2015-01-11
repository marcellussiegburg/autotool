{-# LANGUAGE OverloadedStrings #-}
module Handler.AufgabeEinstellungen where

import Import

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

data Highscore = Hoch | Niedrig deriving (Eq, Show, Read)
instance PathPiece Highscore where
  fromPathPiece t = case reads $ unpack t of
    [(i, "")] -> Just i
    _ -> Nothing
  toPathPiece = pack . show

data Status = Demonstration | Optional | Pflicht deriving (Eq, Show, Read)
instance PathPiece Status where
  fromPathPiece t = case reads $ unpack t of
    [(i, "")] -> Just i
    _ -> Nothing
  toPathPiece = pack . show

highscores ::  [(AutotoolMessage, Maybe Highscore)]
highscores = [(MsgHighscoreKeine, Nothing), (MsgHighscoreHoch, Just Hoch), (MsgHighscoreNiedrig, Just Niedrig)]

stati :: [(AutotoolMessage, Status)]
stati = [(MsgStatusDemonstration, Demonstration), (MsgStatusPflicht, Pflicht), (MsgStatusOptional, Optional)]

aufgabeForm :: Maybe AufgabeFormDaten -> Form AufgabeFormDaten
aufgabeForm maufgabe = renderBootstrap3 BootstrapBasicForm $ aufgabeForm' maufgabe

aufgabeForm' :: Maybe AufgabeFormDaten -> AForm Handler AufgabeFormDaten
aufgabeForm' maufgabe = do
  AufgabeFormDaten
    <$> aopt textField (bfs MsgAufgabeHinweis) (fmap hinweis maufgabe)
    <*> areq (selectFieldList highscores) (bfs MsgAufgabeHighscore) (fmap highscore maufgabe)
    <*> areq (selectFieldList stati) (bfs MsgAufgabeStatus) (fmap status maufgabe)
    <*> areq (jqueryDayField def) (bfsFormControl MsgAufgabeBeginnDatum) (fmap beginn maufgabe)
    <*> areq timeField (bfs MsgAufgabeBeginnZeit) (fmap beginnZeit maufgabe)
    <*> areq (jqueryDayField def) (bfsFormControl MsgAufgabeEndeDatum) (fmap ende maufgabe)
    <*> areq timeField (bfs MsgAufgabeEndeZeit) (fmap endeZeit maufgabe)
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeEinstellen "btn-success" [])
