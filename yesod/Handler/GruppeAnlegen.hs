module Handler.GruppeAnlegen where

import Import
import Handler.Gruppe (gruppeForm)

getGruppeAnlegenR :: VorlesungId -> Handler Html
getGruppeAnlegenR = postGruppeAnlegenR

postGruppeAnlegenR :: VorlesungId -> Handler Html
postGruppeAnlegenR vorlesungId = do
  ((result, formWidget), formEnctype) <- runFormPost $ gruppeForm vorlesungId Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess gruppe -> do
      gruppeId <- runDB $ insert gruppe
      _ <- setMessageI MsgGruppeAngelegt
      redirect $ GruppeR gruppeId
  defaultLayout $
    formToWidget (GruppeAnlegenR vorlesungId) Nothing formEnctype formWidget
