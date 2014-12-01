module Handler.Tutoren where

import Import
import Prelude (undefined)

data TutorListe = TutorEintrag Int Text Text Text Text

getTutorenR :: GruppeId -> Handler Html
getTutorenR gruppe = do
  let tutoren = 
        [TutorEintrag 1 "1234" "Mark" "Otto" "mark.otto@schule.de",
         TutorEintrag 10 "2454" "Jacob" "Thornton" "jacob.thornton@schule.de",
         TutorEintrag 75 "5332" "Larry" "Müller" "larry.mueller@schule.de"
        ]
  defaultLayout $ do
    $(widgetFile "tutoren")

postTutorenR :: GruppeId -> Handler Html
postTutorenR gruppe = undefined
