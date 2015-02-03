module Handler.Vorlesung where

import Import

import Control.Vorlesung.DB as VorlesungDB
import Control.Vorlesung.Typ
import Control.Types

erstellen :: Text
erstellen = "erstellen"

entfernen :: Text
entfernen = "entfernen"

getVorlesungR :: VorlesungId -> Handler Html
getVorlesungR = postVorlesungR

postVorlesungR :: VorlesungId -> Handler Html
postVorlesungR vorlesung = do
  mvorlesung <- lift $ liftM listToMaybe $ VorlesungDB.get_this $ VNr vorlesung
  ((result, formWidget), formEnctype) <- runFormPost $ identifyForm erstellen $ vorlesungForm mvorlesung
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess vorlesung' -> do
      _ <- lift $ VorlesungDB.put (Just $ vnr vorlesung') vorlesung'
      setMessageI MsgVorlesungBearbeitet
  ((entfernenResult, _), _) <- runFormPost $ identifyForm entfernen $ entfernenForm Nothing
  (entfernenWidget, entfernenEnctype) <- case entfernenResult of
    FormMissing ->
      generateFormPost $ identifyForm entfernen $ entfernenForm Nothing
    FormFailure _ ->
      generateFormPost $ identifyForm entfernen $ entfernenForm $ Just entfernen
    FormSuccess _ -> do
      case mvorlesung of
        Nothing -> do
          setMessageI MsgFehler
          redirect SchulenR
        Just vorlesung' -> do
          lift $ VorlesungDB.delete $ vnr vorlesung'
          setMessageI MsgVorlesungEntfernt
          let ENr s = enr vorlesung'
          redirect $ VorlesungenR s
  defaultLayout $ do
    $(widgetFile "vorlesung")

vorlesungForm :: Maybe Vorlesung -> Form Vorlesung
vorlesungForm mvorlesung = do
  let fromName = pack . toString
      toName = Name . unpack
  renderBootstrap3 BootstrapBasicForm $ Vorlesung
    <$> pure (maybe undefined vnr mvorlesung)
    <*> pure (maybe undefined unr mvorlesung)
    <*> pure (maybe undefined enr mvorlesung)
    <*> (toName <$> areq textField (bfs MsgVorlesungName) (fromName . name <$> mvorlesung))
    <*> ((\day time -> utcTimeToTime $ UTCTime day time)
         <$> areq (jqueryDayField def) (bfsFormControl MsgEinschreibungBeginnDatum) (utctDay . timeToUTCTime . einschreibVon <$> mvorlesung)
         <*> (timeOfDayToTime <$> areq timeField (bfs MsgEinschreibungBeginnZeit) (timeToTimeOfDay . utctDayTime . timeToUTCTime . einschreibVon <$> mvorlesung)))
    <*> ((\day time -> utcTimeToTime $ UTCTime day time)
         <$> areq (jqueryDayField def) (bfsFormControl MsgEinschreibungEndeDatum) (utctDay . timeToUTCTime . einschreibBis <$> mvorlesung)
         <*> (timeOfDayToTime <$> areq timeField (bfs MsgEinschreibungEndeZeit) (timeToTimeOfDay . utctDayTime . timeToUTCTime . einschreibBis <$> mvorlesung)))
    <*> pure undefined
    <*> (toName . maybe "" id <$> aopt textField (bfs MsgTagesNachricht) (Just . fromName . motd  <$> mvorlesung))
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgVorlesungAnlegen (\ _ -> MsgVorlesungBearbeiten) mvorlesung) "btn-success" [])

entfernenForm :: Maybe Text -> Form Text
entfernenForm mentfernen = do
  renderBootstrap3 BootstrapBasicForm $
    areq hiddenField (bfs $ maybe MsgLöschen (\_ -> MsgVorlesungEntfernen) mentfernen) mentfernen
    <* bootstrapSubmit (BootstrapSubmit MsgLöschen "btn-danger" [])
