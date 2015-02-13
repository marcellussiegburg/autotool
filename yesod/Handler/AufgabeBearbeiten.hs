module Handler.AufgabeBearbeiten where

import Import
import Handler.AufgabeAnlegen (aufgabeTemplate)
import Control.Aufgabe.DB as AufgabeDB (get_this)
import Control.Types

getAufgabeBearbeitenR :: AufgabeId -> Handler Html
getAufgabeBearbeitenR = postAufgabeBearbeitenR

postAufgabeBearbeitenR :: AufgabeId -> Handler Html
postAufgabeBearbeitenR aufgabeId = do
  aufgaben <- lift $ liftM listToMaybe $ AufgabeDB.get_this $ ANr aufgabeId
  case aufgaben of
    Just aufgabe -> aufgabeTemplate (Right aufgabe)
    Nothing -> notFound
