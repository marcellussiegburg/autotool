module Handler.Resultate where

import Import
import Handler.Studenten (StudentListe(..))

titel :: AutotoolMessage
titel = MsgAlleResultate

links :: VorlesungId -> ((Route Autotool, AutotoolMessage), (Route Autotool, AutotoolMessage))
links vorlesung = ((ResultatePflichtR vorlesung, MsgResultatePflicht), (StudentenR vorlesung, MsgStudenten))

getResultateR :: VorlesungId -> Handler Html
getResultateR vorlesung = do
  let aufgaben = ["guess-the-number", "multiply-peano-numbers", "peano-fold-times"] :: [Text]
      studenten = 
        [StudentEintrag "1234" "Mark" "Otto" 1 0 [Just (1, 0), Just (2, 3), Just (0, 2)],
         StudentEintrag "2454" "Jacob" "Thornton" 2 0 [Just (0, 4), Just (2, 0), Just (0, 0)],
         StudentEintrag "5332" "Larry" "Müller" 1 0 [Just (1,1), Just (5, 0), Nothing]
        ]
  defaultLayout $ do
    $(widgetFile "studenten")
