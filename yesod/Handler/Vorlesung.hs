{-# LANGUAGE OverloadedStrings #-}
module Handler.Vorlesung where

import Import

data VorlesungForm = VorlesungForm {
    name :: Text,
    beginn :: Day,
    beginnZeit :: TimeOfDay,
    ende :: Day,
    endeZeit :: TimeOfDay,
    tagesNachricht :: Maybe Text
}

getVorlesungR :: VorlesungId -> Handler Html
getVorlesungR vorlesung = do
  let beginn' = fromGregorian 2014 9 1
      beginnZeit' = midnight
      ende' = fromGregorian 2014 12 31
      endeZeit' = midnight
      vorlesung' = Just $ VorlesungForm "Theoretische Grundlagen der Informatik" beginn' beginnZeit' ende' endeZeit' $ Just "Keine"
  (formWidget, formEnctype) <- generateFormPost $ vorlesungForm vorlesung'
  defaultLayout $ do
    $(widgetFile "vorlesung")

postVorlesungR :: VorlesungId -> Handler Html
postVorlesungR vorlesung = do
  let beginn' = fromGregorian 2014 9 1
      beginnZeit' = midnight
      ende' = fromGregorian 2014 12 31
      endeZeit' = midnight
      vorlesung' = Just $ VorlesungForm "Theoretische Grundlagen der Informatik" beginn' beginnZeit' ende' endeZeit' $ Just "Keine"
  ((result, formWidget), formEnctype) <- runFormPost $ vorlesungForm vorlesung'
  defaultLayout $ do
    $(widgetFile "vorlesung")

vorlesungForm :: Maybe VorlesungForm -> Form VorlesungForm
vorlesungForm mvorlesung = do
  renderBootstrap3 BootstrapBasicForm $ VorlesungForm
    <$> areq textField (bfs MsgVorlesungName) (fmap name mvorlesung)
    <*> areq (jqueryDayField def) (bfsFormControl MsgEinschreibungBeginnDatum) (fmap beginn mvorlesung)
    <*> areq timeField (bfs MsgEinschreibungBeginnZeit) (fmap beginnZeit mvorlesung)
    <*> areq (jqueryDayField def) (bfsFormControl MsgEinschreibungEndeDatum) (fmap ende mvorlesung)
    <*> areq timeField (bfs MsgEinschreibungEndeZeit) (fmap endeZeit mvorlesung)
    <*> aopt textField (bfs MsgTagesNachricht) (fmap tagesNachricht mvorlesung)
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgVorlesungAnlegen (\ _ -> MsgVorlesungBearbeiten) mvorlesung) "btn-success" [])
    <* maybe (pure ()) (\_ -> bootstrapSubmit (BootstrapSubmit MsgLöschen "btn-danger" [])) mvorlesung
