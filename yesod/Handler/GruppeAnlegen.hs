module Handler.GruppeAnlegen where

import Import
import Handler.Gruppe (gruppeForm)

import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Vorlesung.Typ as Vorlesung
import qualified Control.Gruppe.DB as GruppeDB
import Control.Gruppe.Typ
import Control.Types

getGruppeAnlegenR :: VorlesungId -> Handler Html
getGruppeAnlegenR = postGruppeAnlegenR

postGruppeAnlegenR :: VorlesungId -> Handler Html
postGruppeAnlegenR vorlesungId = do
  ((result, formWidget), formEnctype) <- runFormPost $ gruppeForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess gruppe' -> do
      Just vorlesung' <- runDB $ get vorlesungId
      _ <- lift $ GruppeDB.put Nothing gruppe' { vnr = VNr $ keyToInt vorlesungId }
      _ <- setMessageI MsgGruppeAngelegt
      redirect $ GruppenR vorlesungId -- TODO: GruppeR verwenden (zu neu erstellter Übungsgruppe gehen)
  defaultLayout $
    formToWidget (GruppeAnlegenR vorlesungId) Nothing formEnctype formWidget
