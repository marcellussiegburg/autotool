module Handler.Aufgabe where

import Import
import Handler.AufgabeAnlegen (aufgabeTemplate)
import Control.Aufgabe.DB as AufgabeDB (get_this)
import Control.Types

getAufgabeR :: AufgabeId -> Handler Html
getAufgabeR = postAufgabeR

postAufgabeR :: AufgabeId -> Handler Html
postAufgabeR aufgabeId = do
  aufgaben <- lift $ liftM listToMaybe $ AufgabeDB.get_this $ ANr aufgabeId
  case aufgaben of
    Just aufgabe -> aufgabeTemplate (Right aufgabe)
    Nothing -> notFound
