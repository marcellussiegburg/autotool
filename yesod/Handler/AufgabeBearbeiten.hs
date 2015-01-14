module Handler.AufgabeBearbeiten where

import Import
import Handler.AufgabeAnlegen (aufgabeTemplate)
import Handler.AufgabeEinstellungen

getAufgabeBearbeitenR :: AufgabeId -> Handler Html
getAufgabeBearbeitenR aufgabe = do
  let beginn' = fromGregorian 2014 10 1
      beginnZeit' = midnight
      ende' = fromGregorian 2015 3 31
      endeZeit' = midnight
      aufgabe' = Just $ AufgabeFormDaten (Just "kein Hinweis") Nothing Pflicht beginn' beginnZeit' ende' endeZeit'
  aufgabeTemplate (Right aufgabe) $ Just (Just "http://server.url", Just "Unify-Direct-1", aufgabe', Just Nothing, Just "h (a (c , g (d , k )), l (m , f (i , b )))")

postAufgabeBearbeitenR :: AufgabeId -> Handler Html
postAufgabeBearbeitenR aufgabe = aufgabeTemplate (Right aufgabe) Nothing
