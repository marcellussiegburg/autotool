module Handler.Statistik where

import Import

data ErgebisListe = ErgebnisEintrag Text Text Text Int Int (Maybe Ergebnis)
data Ergebnis = Okay {punkte :: Int, größe :: Int} | Nein | Ausstehend

getStatistikR :: AufgabeId -> Handler Html
getStatistikR aufgabe = do
  let ergebnisse = 
        [ErgebnisEintrag "1234" "Mark" "Otto" 1 0 (Just $ Okay 1 2),
         ErgebnisEintrag "2454" "Jacob" "Thornton" 0 0 Nothing,
         ErgebnisEintrag "2454" "Roy" "Meyer" 0 3 (Just Nein),
         ErgebnisEintrag "5332" "Larry" "Müller" 0 2 (Just Ausstehend)
        ]
  defaultLayout $ do
    $(widgetFile "statistik")
    
postStatistikR :: AufgabeId -> Handler Html
postStatistikR = undefined
