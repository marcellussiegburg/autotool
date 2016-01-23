module Handler.AufgabeVorlage where

import Import
import Handler.AufgabeKonfiguration (getBeispielKonfiguration)
import Control.Aufgabe.Typ (Aufgabe (name, config))
import Control.Types

getAufgabeVorlageR :: ServerUrl -> AufgabeTyp -> VorlageName -> Handler Html
getAufgabeVorlageR server aufgabeTyp vorlageName = do
  beispielKonfiguration <- liftM fst $ getBeispielKonfiguration server aufgabeTyp
  konfiguration <- getVorlageKonfiguration beispielKonfiguration aufgabeTyp vorlageName
  redirect $ AufgabeKonfigurationR server aufgabeTyp konfiguration

getVorlageKonfiguration :: AufgabeKonfiguration -> AufgabeTyp -> VorlageName -> Handler AufgabeKonfiguration
getVorlageKonfiguration beispielKonfiguration aufgabeTyp vorlageName = do
  aufgaben <- runDB $ selectList [AufgabeTyp ==. aufgabeTyp, AufgabeName ==. vorlageName] []
  case listToMaybe aufgaben of
    Nothing -> return beispielKonfiguration
    Just aufgabe -> return $ aufgabeKonfiguration $ entityVal aufgabe
