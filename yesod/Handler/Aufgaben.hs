module Handler.Aufgaben where

import Import

data AufgabeListe = AufgabeEintrag {
    name :: Text,
    status :: Text,
    highscore :: Text,
    bearbeitungszeit :: UTCTime,
    vorherigeBewertung :: Maybe Text,
    gesamtBewertung :: Maybe (Int, Int)
}

getAufgabenR :: GruppeId -> Handler Html
getAufgabenR gruppe = do
  let aufgaben =
        [AufgabeEintrag "Reconstruct-Direct-1" "Demo" "Keine" (UTCTime (fromGregorian 2014 12 31) $ timeOfDayToTime midnight) (Just "Pending") (Just (0,4)),
         AufgabeEintrag "Reconstruct-Direct-1" "Optional" "Niedrig" (UTCTime (fromGregorian 2014 10 31) $ timeOfDayToTime midnight) (Just "Okay") (Just (2,0)),
         AufgabeEintrag "Reconstruct-Direct-1" "Pflicht" "Hoch" (UTCTime (fromGregorian 2014 12 31) $ timeOfDayToTime midnight) Nothing (Just (0,0))
        ]
  defaultLayout $ do
    $(widgetFile "aufgaben")
