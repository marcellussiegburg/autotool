{-# LANGUAGE OverloadedStrings #-}
module Handler.Vorlesung where

import Import

data VorlesungForm = VorlesungForm {
    name :: Text,
    semester :: Int,
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
      vorlesung' = Just $ VorlesungForm "Theoretische Grundlagen der Informatik" 1 beginn' beginnZeit' ende' endeZeit' $ Just "Keine"
  (formWidget, formEnctype) <- generateFormPost $ vorlesungForm vorlesung'
  defaultLayout $ do
    $(widgetFile "vorlesung")

postVorlesungR :: VorlesungId -> Handler Html
postVorlesungR vorlesung = do
  let beginn' = fromGregorian 2014 9 1
      beginnZeit' = midnight
      ende' = fromGregorian 2014 12 31
      endeZeit' = midnight
      vorlesung' = Just $ VorlesungForm "Theoretische Grundlagen der Informatik" 1 beginn' beginnZeit' ende' endeZeit' $ Just "Keine"
  ((result, formWidget), formEnctype) <- runFormPost $ vorlesungForm vorlesung'
  defaultLayout $ do
    $(widgetFile "vorlesung")

vorlesungForm :: Maybe VorlesungForm -> Form VorlesungForm
vorlesungForm mvorlesung = do
  let vorlesungen = [("WS 2014/2015", 1), ("SS 2014", 2), ("WS 2013/2014", 3), ("SS 2013", 4), ("WS 2012/2013", 5)] :: [(Text, Int)]
  renderBootstrap3 BootstrapBasicForm $ VorlesungForm
    <$> areq textField (bfs MsgVorlesungName) (fmap name mvorlesung)
    <*> areq (selectFieldList vorlesungen) (bfs MsgSemester) (fmap semester mvorlesung)
    <*> areq (jqueryDayField def) (bfsFormControl MsgEinschreibungBeginnDatum) (fmap beginn mvorlesung)
    <*> areq timeField (bfs MsgEinschreibungBeginnZeit) (fmap beginnZeit mvorlesung)
    <*> areq (jqueryDayField def) (bfsFormControl MsgEinschreibungEndeDatum) (fmap ende mvorlesung)
    <*> areq timeField (bfs MsgEinschreibungEndeZeit) (fmap endeZeit mvorlesung)
    <*> aopt textField (bfs MsgTagesNachricht) (fmap tagesNachricht mvorlesung)
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgVorlesungAnlegen (\ _ -> MsgVorlesungBearbeiten) mvorlesung) "btn-success" [])
