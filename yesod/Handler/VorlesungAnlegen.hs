module Handler.VorlesungAnlegen where

import Import
import Handler.Vorlesung (vorlesungForm)

getVorlesungAnlegenR :: SchuleId -> Handler Html
getVorlesungAnlegenR schule = do
  (formWidget, formEnctype) <- generateFormPost $ vorlesungForm Nothing
  defaultLayout $ do
    $(widgetFile "vorlesungAnlegen")

postVorlesungAnlegenR :: SchuleId -> Handler Html
postVorlesungAnlegenR schule = do
  ((result, formWidget), formEnctype) <- runFormPost $ vorlesungForm Nothing
  defaultLayout $ do
    $(widgetFile "vorlesungAnlegen")
