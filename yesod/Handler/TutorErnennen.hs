module Handler.TutorErnennen where

import Import
import Prelude (undefined)

data TutorListe = TutorEintrag Int Text Text Text Text

title :: AutotoolMessage
title = MsgTutor

getTutorErnennenR :: GruppeId -> Handler Html
getTutorErnennenR gruppe = do
  let tutoren = 
        [TutorEintrag 1 "1234" "Mark" "Otto" "mark.otto@schule.de",
         TutorEintrag 10 "2454" "Jacob" "Thornton" "jacob.thornton@schule.de",
         TutorEintrag 75 "5332" "Larry" "Müller" "larry.mueller@schule.de"
        ]
      ernennen = False
  defaultLayout $ do
    $(widgetFile "tutoren")

postTutorErnennenR :: GruppeId -> Handler Html
postTutorErnennenR gruppe = undefined
