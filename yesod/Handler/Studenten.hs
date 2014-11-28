module Handler.Studenten where

import Import

titel :: AutotoolMessage
titel = MsgStudenten

links :: VorlesungId -> ((Route Autotool, AutotoolMessage), (Route Autotool, AutotoolMessage))
links vorlesung = ((ResultateR vorlesung, MsgAlleResultate), (ResultatePflichtR vorlesung, MsgResultatePflicht))

getStudentenR :: VorlesungId -> Handler Html
getStudentenR vorlesung = do
  let aufgaben = [] :: [Text]
      studenten = 
        [StudentEintrag "1234" "Mark" "Otto" 1 0 [],
         StudentEintrag "2454" "Jacob" "Thornton" 2 0 [],
         StudentEintrag "5332" "Larry" "Müller" 1 0 []
        ]
  defaultLayout $ do
    $(widgetFile "studenten")
