module Handler.Aufgabe where

import Import
import Handler.AufgabeAnlegen (aufgabeTemplate)

getAufgabeR :: AufgabeId -> Handler Html
getAufgabeR = postAufgabeR

postAufgabeR :: AufgabeId -> Handler Html
postAufgabeR aufgabeId = do
  runDB $ get404 aufgabeId
  aufgabeTemplate (Right aufgabeId)
