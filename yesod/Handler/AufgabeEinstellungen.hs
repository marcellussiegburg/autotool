module Handler.AufgabeEinstellungen where

import Import hiding (delete)
import Handler.AufgabeVorlagen (getVorlagen)

import Data.List (delete)

import qualified Control.Aufgabe.Typ as A
import qualified Control.Types as T

data AufgabeFormDaten = AufgabeFormDaten {
    name :: Text,
    hinweis :: Maybe Text,
    highscore :: T.HiLo,
    status :: T.Status,
    beginn :: UTCTime,
    ende :: UTCTime
} deriving (Show, Read)

instance PathPiece AufgabeFormDaten where
  fromPathPiece t = case reads $ unpack t of
      [(i, "")] -> Just i
      _ -> Nothing
  toPathPiece = pack . show

aufgabeToFormDaten :: Aufgabe -> AufgabeFormDaten
aufgabeToFormDaten aufgabe =
  AufgabeFormDaten {
    name = aufgabeName aufgabe,
    hinweis = aufgabeHinweis aufgabe,
    highscore = aufgabeHighscore aufgabe,
    status = aufgabeStatus aufgabe,
    beginn = aufgabeVon aufgabe,
    ende = aufgabeBis aufgabe
  }

highscores ::  [(AutotoolMessage, T.HiLo)]
highscores = [(MsgHighscoreKeine, T.Keine), (MsgHighscoreHoch, T.High), (MsgHighscoreNiedrig, T.Low)]

stati :: [(AutotoolMessage, T.Status)]
stati = [(MsgStatusDemonstration, T.Demo), (MsgStatusPflicht, T.Mandatory), (MsgStatusOptional, T.Optional)]

aufgabeForm :: AufgabeTyp -> Maybe AufgabeFormDaten -> AForm Handler AufgabeFormDaten
aufgabeForm aufgabeTyp maufgabe = AufgabeFormDaten
    <$> areq nameField (bfs MsgAufgabeName) (name <$> maufgabe)
    <*> (fmap unTextarea <$> aopt textareaField (bfs MsgAufgabeHinweis) (fmap Textarea . hinweis <$> maufgabe))
    <*> areq (selectFieldList highscores) (bfs MsgAufgabeHighscore) (highscore <$> maufgabe)
    <*> areq (selectFieldList stati) (bfs MsgAufgabeStatus) (status <$> maufgabe)
    <*> (UTCTime
         <$> areq (jqueryDayField def) (bfsFormControl MsgAufgabeBeginnDatum) (utctDay . beginn <$> maufgabe)
         <*> (timeOfDayToTime <$> areq timeFieldTypeTime (bfs MsgAufgabeBeginnZeit) (timeToTimeOfDay . utctDayTime . beginn <$> maufgabe)))
    <*> (UTCTime
         <$> areq (jqueryDayField def) (bfsFormControl MsgAufgabeEndeDatum) (utctDay . ende <$> maufgabe)
         <*> (timeOfDayToTime <$> areq timeFieldTypeTime (bfs MsgAufgabeEndeZeit) (timeToTimeOfDay . utctDayTime . ende <$> maufgabe)))
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeEinstellen "btn-success" [])
  where
    nameField = flip checkM textField $ \ n -> do
      vorlagen <- getVorlagen aufgabeTyp
      let vorlagen' = maybe vorlagen ((flip delete vorlagen) . name) maufgabe
      if n `elem` vorlagen'
        then return $ Left MsgAufgabeNameVergeben
        else return $ Right n
