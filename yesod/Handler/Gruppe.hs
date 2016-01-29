module Handler.Gruppe where

import Import

erstellen :: Text
erstellen = "erstellen"

entfernen :: Text
entfernen = "entfernen"

getGruppeR :: GruppeId -> Handler Html
getGruppeR = postGruppeR

postGruppeR :: GruppeId -> Handler Html
postGruppeR gruppeId = do
  gruppe <- runDB $ get404 gruppeId
  ((result, formWidget), formEnctype) <- runFormPost $ identifyForm erstellen $ gruppeForm (gruppeVorlesungId gruppe) $ Just gruppe
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess gruppe' -> do
      _ <- runDB $ replace gruppeId gruppe'
      setMessageI MsgGruppeBearbeitet
  ((entfernenResult, _), _) <- runFormPost $ identifyForm entfernen $ entfernenForm Nothing
  (entfernenWidget, entfernenEnctype) <- case entfernenResult of
    FormMissing ->
      generateFormPost $ identifyForm entfernen $ entfernenForm Nothing
    FormFailure _ ->
      generateFormPost $ identifyForm entfernen $ entfernenForm $ Just entfernen
    FormSuccess _ -> do
      runDB $ delete gruppeId
      setMessageI MsgGruppeEntfernt
      redirect $ GruppenR $ gruppeVorlesungId gruppe
  defaultLayout $
    $(widgetFile "gruppe")

gruppeForm :: VorlesungId -> Maybe Gruppe -> Form Gruppe
gruppeForm vorlesungId mgruppe = do
    renderBootstrap3 BootstrapBasicForm $ Gruppe
      <$> pure vorlesungId
      <*> areq textField (bfs MsgGruppeName) (gruppeName <$> mgruppe)
      <*> areq mitgliederField (bfs MsgPlätze) (gruppePlaetze <$> mgruppe)
      <*> areq textField (bfs MsgReferent) (gruppeReferent <$> mgruppe)
      <* bootstrapSubmit (BootstrapSubmit (maybe MsgGruppeAnlegen (\ _ -> MsgGruppeBearbeiten) mgruppe) "btn-success" [])
  where
    mitgliederField = checkBool (> 0) MsgGruppeZuKlein intField

entfernenForm :: Maybe Text -> Form Text
entfernenForm mentfernen = do
  renderBootstrap3 BootstrapBasicForm $
    areq hiddenField (bfs $ maybe MsgLöschen (\_ -> MsgGruppeEntfernen) mentfernen) mentfernen
    <* bootstrapSubmit (BootstrapSubmit MsgLöschen "btn-danger" [])
