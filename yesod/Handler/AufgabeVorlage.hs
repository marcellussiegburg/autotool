module Handler.AufgabeVorlage where

import qualified Control.Exception as Exception
import Import
import Handler.AufgabeKonfiguration (getBeispielKonfiguration)
import qualified Control.Aufgabe.DB as AufgabeDB
import Control.Aufgabe.Typ (Aufgabe (name, config))
import Control.Types

getAufgabeVorlageR :: ServerUrl -> AufgabeTyp -> VorlageName -> Handler Html
getAufgabeVorlageR server aufgabeTyp vorlageName = do
  beispielKonfiguration <- liftM fst $ getBeispielKonfiguration server aufgabeTyp
  konfiguration <- getVorlageKonfiguration beispielKonfiguration aufgabeTyp vorlageName
  redirect $ AufgabeKonfigurationR server aufgabeTyp konfiguration

getVorlageKonfiguration :: AufgabeKonfiguration -> AufgabeTyp -> VorlageName -> Handler AufgabeKonfiguration
getVorlageKonfiguration beispielKonfiguration aufgabeTyp vorlageName = do
  aufgaben <- lift $ AufgabeDB.get_typed (fromCGI $ unpack aufgabeTyp)
      `Exception.catch` \ (Exception.SomeException _) -> return []
  let maufgabe = listToMaybe $ filter (\a -> vorlageName == (pack $ toString $ name a)) aufgaben
  case maufgabe of
    Nothing -> return beispielKonfiguration
    Just aufgabe -> return $ pack $ toString $ config aufgabe
