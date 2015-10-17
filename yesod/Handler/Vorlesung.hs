module Handler.Vorlesung where

import Import

erstellen :: Text
erstellen = "erstellen"

entfernen :: Text
entfernen = "entfernen"

getVorlesungR :: VorlesungId -> Handler Html
getVorlesungR = postVorlesungR

postVorlesungR :: VorlesungId -> Handler Html
postVorlesungR vorlesungId = do
  vorlesung <- runDB $ get404 vorlesungId
  ((result, formWidget), formEnctype) <- runFormPost $ identifyForm erstellen $ vorlesungForm $ Just vorlesung
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess vorlesung' -> do
      _ <- runDB $ replace vorlesungId vorlesung'
      setMessageI MsgVorlesungBearbeitet
  ((entfernenResult, _), _) <- runFormPost $ identifyForm entfernen $ entfernenForm Nothing
  (entfernenWidget, entfernenEnctype) <- case entfernenResult of
    FormMissing ->
      generateFormPost $ identifyForm entfernen $ entfernenForm Nothing
    FormFailure _ ->
      generateFormPost $ identifyForm entfernen $ entfernenForm $ Just entfernen
    FormSuccess _ -> do
      runDB $ delete vorlesungId
      setMessageI MsgVorlesungEntfernt
      redirect $ VorlesungenR $ vorlesungSemesterId vorlesung
  defaultLayout $
    $(widgetFile "vorlesung")

vorlesungForm :: Maybe Vorlesung -> Form Vorlesung
vorlesungForm mvorlesung = do
  renderBootstrap3 BootstrapBasicForm $ Vorlesung
    <$> pure (maybe undefined vorlesungSchuleId mvorlesung)
    <*> pure (maybe undefined vorlesungSemesterId mvorlesung)
    <*> (areq textField (bfs MsgVorlesungName) (vorlesungName <$> mvorlesung))
    <*> (UTCTime
         <$> areq (jqueryDayField def) (bfsFormControl MsgEinschreibungBeginnDatum) (utctDay . vorlesungVon <$> mvorlesung)
         <*> (timeOfDayToTime <$> areq timeFieldTypeTime (bfs MsgEinschreibungBeginnZeit) (timeToTimeOfDay . utctDayTime . vorlesungVon <$> mvorlesung)))
    <*> (UTCTime
         <$> areq (jqueryDayField def) (bfsFormControl MsgEinschreibungEndeDatum) (utctDay . vorlesungBis <$> mvorlesung)
         <*> (timeOfDayToTime <$> areq timeFieldTypeTime (bfs MsgEinschreibungEndeZeit) (timeToTimeOfDay . utctDayTime . vorlesungBis <$> mvorlesung)))
    <*> aopt textField (bfs MsgTagesNachricht) (vorlesungNachricht  <$> mvorlesung)
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgVorlesungAnlegen (\ _ -> MsgVorlesungBearbeiten) mvorlesung) "btn-success" [])

entfernenForm :: Maybe Text -> Form Text
entfernenForm mentfernen = do
  renderBootstrap3 BootstrapBasicForm $
    areq hiddenField (bfs $ maybe MsgLöschen (\_ -> MsgVorlesungEntfernen) mentfernen) mentfernen
    <* bootstrapSubmit (BootstrapSubmit MsgLöschen "btn-danger" [])
