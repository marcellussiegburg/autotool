module Handler.AufgabenAktuell where

import Import

import Handler.Aufgaben (AufgabeListe(..))

titel = MsgAufgabenAktuell

aktuell = True

getAufgabenAktuellR :: VorlesungId -> Handler Html
getAufgabenAktuellR vorlesung = do
  let aufgaben =
        [AufgabeEintrag "Reconstruct-Direct-1" "Demo" "Keine" (UTCTime (fromGregorian 2014 12 31) $ timeOfDayToTime midnight) (Just "Pending") (Just (0,4)),
         AufgabeEintrag "Reconstruct-Direct-1" "Pflicht" "Hoch" (UTCTime (fromGregorian 2014 12 31) $ timeOfDayToTime midnight) Nothing (Just (0,0))
        ]
  defaultLayout $ do
    $(widgetFile "aufgaben")
