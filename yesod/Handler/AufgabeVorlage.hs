module Handler.AufgabeVorlage where

import qualified Control.Exception as Exception
import Import
import qualified Control.Aufgabe.DB as AufgabeDB
import Control.Aufgabe.Typ (Aufgabe (name, config))
import Control.Types
import Service.Interface (get_task_description_localized)
import Types.Config (Config (CString))
import Types.Documented (contents)
import Types.TaskDescription (task_sample_config)

getAufgabeVorlageR :: ServerUrl -> AufgabeTyp -> VorlageName -> Handler Html
getAufgabeVorlageR server aufgabeTyp vorlageName = do
  aufgaben <- lift $ AufgabeDB.get_typed (fromCGI $ unpack aufgabeTyp)
      `Exception.catch` \ (Exception.SomeException _) -> return []
  let maufgabe = listToMaybe $ filter (\a -> vorlageName == (pack $ toString $ name a)) aufgaben
  sprache <- getBevorzugteSprache
  CString konfiguration <- case maufgabe of
    Nothing -> lift $ liftM (contents . task_sample_config)
                    $ get_task_description_localized (unpack server) (unpack aufgabeTyp) sprache
    Just aufgabe -> return $ CString $ toString $ config aufgabe
  redirect $ AufgabeKonfigurationR server aufgabeTyp $ pack konfiguration
