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
postGruppeAnlegenR vorlesung = do
  ((result, formWidget), formEnctype) <- runFormPost $ gruppeForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess gruppe' -> do
      Just vorlesung' <- lift $ liftM listToMaybe $ VorlesungDB.get_this $ VNr vorlesung
      _ <- lift $ GruppeDB.put Nothing gruppe' { vnr = Vorlesung.vnr vorlesung' }
      _ <- setMessageI MsgGruppeAngelegt
      let VNr v = Vorlesung.vnr vorlesung'
      redirect $ GruppenR v -- ^ TODO: GruppeR verwenden (zu neu erstellter Übungsgruppe gehen)
  defaultLayout $ do
    $(widgetFile "gruppeAnlegen")
