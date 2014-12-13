module Handler.Vorlesungen where

import Import

import Handler.Vorlesung (VorlesungForm (..))

getVorlesungenR :: SemesterId -> Handler Html
getVorlesungenR semester = do
  let neuesDatum datum = UTCTime datum $ timeOfDayToTime midnight
      vorlesungen =
        [VorlesungForm "Theoretische Grundlagen der Informatik" (fromGregorian 2014 9 31) midnight (fromGregorian 2014 12 31) midnight (Just "Keine"),
         VorlesungForm "Compilerbau" (fromGregorian 2014 10 31) midnight (fromGregorian 2014 12 31) midnight Nothing,
         VorlesungForm "Constraint Programmierung" (fromGregorian 2014 8 31) midnight (fromGregorian 2014 12 31) midnight (Just "Bitte einschreiben"),
         VorlesungForm "Symbolisches Rechnen" (fromGregorian 2014 6 31) midnight (fromGregorian 2014 9 31) midnight (Just "Einschreibung vorbei")]
  defaultLayout $ do
    $(widgetFile "vorlesungen")

postVorlesungenR :: SemesterId -> Handler Html
postVorlesungenR = getVorlesungenR

listGroupItemClass :: Day -> Text
listGroupItemClass datum =
   if datum == fromGregorian 2014 12 31
   then "list-group-item-success"
   else "list-group-item-warning"
