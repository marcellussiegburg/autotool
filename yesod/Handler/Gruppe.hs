module Handler.Gruppe where

import Import

import qualified Control.Gruppe.DB as GruppeDB
import Control.Gruppe.Typ
import Control.Types

erstellen :: Text
erstellen = "erstellen"

entfernen :: Text
entfernen = "entfernen"

getGruppeR :: GruppeId -> Handler Html
getGruppeR = postGruppeR

postGruppeR :: GruppeId -> Handler Html
postGruppeR gruppe = do
  mgruppe <- lift $ liftM listToMaybe $ GruppeDB.get_gnr $ GNr gruppe
  ((result, formWidget), formEnctype) <- runFormPost $ identifyForm erstellen $ gruppeForm mgruppe
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess gruppe' -> do
      _ <- lift $ GruppeDB.put (Just $ gnr gruppe') gruppe'
      setMessageI MsgGruppeBearbeitet
  ((entfernenResult, _), _) <- runFormPost $ identifyForm entfernen $ entfernenForm Nothing
  (entfernenWidget, entfernenEnctype) <- case entfernenResult of
    FormMissing ->
      generateFormPost $ identifyForm entfernen $ entfernenForm Nothing
    FormFailure _ ->
      generateFormPost $ identifyForm entfernen $ entfernenForm $ Just entfernen
    FormSuccess _ -> do
      case mgruppe of
        Nothing -> do
          setMessageI MsgFehler
          redirect SchulenR
        Just gruppe' -> do
          lift $ GruppeDB.delete $ gnr gruppe'
          setMessageI MsgGruppeEntfernt
          let VNr s = vnr gruppe'
          redirect $ GruppenR s
  defaultLayout $ do
    $(widgetFile "gruppe")

gruppeForm :: Maybe Gruppe -> Form Gruppe
gruppeForm mgruppe = do
    let fromName = pack . toString
        toName = Name . unpack
    renderBootstrap3 BootstrapBasicForm $ Gruppe
      <$> pure (maybe undefined gnr mgruppe)
      <*> pure (maybe undefined vnr mgruppe)
      <*> (toName <$> areq textField (bfs MsgGruppeName) (fromName . name <$> mgruppe))
      <*> areq mitgliederField (bfs MsgPlätze) (maxStudents <$> mgruppe)
      <*> (toName <$> areq textField (bfs MsgReferent) (fromName . referent <$> mgruppe))
      <* bootstrapSubmit (BootstrapSubmit (maybe MsgGruppeAnlegen (\ _ -> MsgGruppeBearbeiten) mgruppe) "btn-success" [])
  where
    mitgliederField = checkBool (> 0) MsgGruppeZuKlein intField

entfernenForm :: Maybe Text -> Form Text
entfernenForm mentfernen = do
  renderBootstrap3 BootstrapBasicForm $
    areq hiddenField (bfs $ maybe MsgLöschen (\_ -> MsgGruppeEntfernen) mentfernen) mentfernen
    <* bootstrapSubmit (BootstrapSubmit MsgLöschen "btn-danger" [])
