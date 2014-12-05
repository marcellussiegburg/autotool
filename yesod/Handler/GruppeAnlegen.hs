module Handler.GruppeAnlegen where

import Import
import Handler.Gruppe (gruppeForm)

getGruppeAnlegenR :: VorlesungId -> Handler Html
getGruppeAnlegenR vorlesung = do
  (formWidget, formEnctype) <- generateFormPost $ gruppeForm Nothing
  defaultLayout $ do
    $(widgetFile "gruppeAnlegen")

postGruppeAnlegenR :: VorlesungId -> Handler Html
postGruppeAnlegenR vorlesung = do
  ((result, formWidget), formEnctype) <- runFormPost $ gruppeForm Nothing
  defaultLayout $ do
    $(widgetFile "gruppeAnlegen")
