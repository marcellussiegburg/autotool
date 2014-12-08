module Handler.Direktoren where

import Import
import Prelude (undefined)

data StudentListe = StudentEintrag Int Text Text Text Text

title :: AutotoolMessage
title = MsgDirektoren

getDirektorenR :: GruppeId -> Handler Html
getDirektorenR gruppe = do
  let studenten = 
        [StudentEintrag 1 "1234" "Mark" "Otto" "mark.otto@schule.de",
         StudentEintrag 10 "2454" "Jacob" "Thornton" "jacob.thornton@schule.de",
         StudentEintrag 75 "5332" "Larry" "Müller" "larry.mueller@schule.de"
        ]
      ernennen = False
      label = MsgDirektorAbsetzen
      nullStudenten = MsgKeineDirektoren
  defaultLayout $ do
    $(widgetFile "studentenFunktion")

postDirektorenR :: GruppeId -> Handler Html
postDirektorenR gruppe = undefined
