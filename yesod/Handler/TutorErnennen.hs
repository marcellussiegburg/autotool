module Handler.TutorErnennen where

import Import
import Handler.Tutoren (StudentListe (StudentEintrag))

title :: AutotoolMessage
title = MsgTutor

getTutorErnennenR :: VorlesungId -> Handler Html
getTutorErnennenR vorlesung = do
  let studenten = 
        [StudentEintrag 1 "1234" "Mark" "Otto" "mark.otto@schule.de",
         StudentEintrag 10 "2454" "Jacob" "Thornton" "jacob.thornton@schule.de",
         StudentEintrag 75 "5332" "Larry" "Müller" "larry.mueller@schule.de"
        ]
      ernennen = True
      label = MsgTutorErnennen
      nullStudenten = MsgKeineStudentenErnennen
  defaultLayout $ do
    $(widgetFile "studentenFunktion")

postTutorErnennenR :: VorlesungId -> Handler Html
postTutorErnennenR vorlesung = undefined
