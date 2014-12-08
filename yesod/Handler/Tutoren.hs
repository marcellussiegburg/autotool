module Handler.Tutoren where

import Import
import Prelude (undefined)

data StudentListe = StudentEintrag Int Text Text Text Text

title :: AutotoolMessage
title = MsgTutoren

getTutorenR :: GruppeId -> Handler Html
getTutorenR gruppe = do
  let studenten = 
        [StudentEintrag 1 "1234" "Mark" "Otto" "mark.otto@schule.de",
         StudentEintrag 10 "2454" "Jacob" "Thornton" "jacob.thornton@schule.de",
         StudentEintrag 75 "5332" "Larry" "Müller" "larry.mueller@schule.de"
        ]
      ernennen = False
      label = MsgTutorAbsetzen
      nullStudenten = MsgKeineTutoren
  defaultLayout $ do
    $(widgetFile "studentenFunktion")

postTutorenR :: GruppeId -> Handler Html
postTutorenR gruppe = undefined
