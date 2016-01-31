module Handler.AufgabeVorlage where

import Import
import Handler.AufgabeKonfiguration (getBeispielKonfiguration)

getAufgabeVorlageR :: ServerUrl -> AufgabeTyp -> VorlageName -> Handler Html
getAufgabeVorlageR server aTyp vorlageName = do
  beispielKonfiguration <- liftM fst $ getBeispielKonfiguration server aTyp
  konfiguration <- getVorlageKonfiguration beispielKonfiguration aTyp vorlageName
  redirect $ AufgabeKonfigurationR server aTyp konfiguration

getVorlageKonfiguration :: AufgabeKonfiguration -> AufgabeTyp -> VorlageName -> Handler AufgabeKonfiguration
getVorlageKonfiguration beispielKonfiguration aTyp vorlageName = do
  aufgaben <- runDB $ selectList [AufgabeTyp ==. aTyp, AufgabeName ==. vorlageName] []
  case listToMaybe aufgaben of
    Nothing -> return beispielKonfiguration
    Just aufgabe -> return $ aufgabeKonfiguration $ entityVal aufgabe
