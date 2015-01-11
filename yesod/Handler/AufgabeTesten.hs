module Handler.AufgabeTesten where

import Import

import Handler.Aufgabe

getAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeTestenR = postAufgabeTestenR

postAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeTestenR server typ konfiguration = do
  let vorherigeEinsendung = Just $ "h (a (c , g (d , k )), l (m , f (i , b )))"
  ((result, formWidget), formEnctype) <- runFormPost $ identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $ aufgabeLösenForm vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm $ lösungHochladenForm
  let mbewertung = Just bew
      widgets = (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload)
  aufgabeTemplate Testen (AufgabeTestenR server typ konfiguration) widgets mbewertung
