module Handler.AufgabeVorlagen where

import Import
import qualified Control.Aufgabe.DB as AufgabeDB
import Control.Aufgabe (Aufgabe (name))
import qualified Control.Exception as Exception
import Control.Types

getAufgabeVorlagenR :: ServerUrl -> AufgabeTyp -> Handler Html
getAufgabeVorlagenR server aufgabeTyp = do
  aufgaben <- lift $ AufgabeDB.get_typed (fromCGI $ unpack aufgabeTyp)
    `Exception.catch` \ (Exception.SomeException _) -> return []
  let vorlagen = map (pack . toString . name) aufgaben
  defaultLayout $ do
    $(widgetFile "aufgabeVorlagen")
