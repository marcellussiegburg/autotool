module Handler.ResultatePflicht where

import Import 
import Handler.Studenten (StudentListe(..))

titel :: AutotoolMessage
titel = MsgResultatePflicht

links :: VorlesungId -> ((Route Autotool, AutotoolMessage), (Route Autotool, AutotoolMessage))
links vorlesung = ((StudentenR vorlesung, MsgStudenten), (ResultateR vorlesung, MsgAlleResultate))

getResultatePflichtR :: VorlesungId -> Handler Html
getResultatePflichtR vorlesung = do
  let aufgaben = ["peano-fold-times"] :: [Text]
      studenten = 
        [StudentEintrag "1234" "Mark" "Otto" 1 0 [Just (0, 2)],
         StudentEintrag "2454" "Jacob" "Thornton" 2 0 [Just (0, 0)],
         StudentEintrag "5332" "Larry" "Müller" 1 0 [Nothing]
        ]
  defaultLayout $ do
    $(widgetFile "studenten")
