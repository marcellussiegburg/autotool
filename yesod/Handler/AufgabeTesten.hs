module Handler.AufgabeTesten where

import Import

import Handler.Aufgabe

getAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeTestenR server typ konfiguration = do
  let vorherigeEinsendung = Just $ Textarea "h (a (c , g (d , k )), l (m , f (i , b )))"
  (formWidget, formEnctype) <- generateFormPost $ aufgabeLösenForm vorherigeEinsendung
  (formWidgetUpload, formEnctypeUpload) <- generateFormPost lösungHochladenForm
  let mbewertung = Nothing
      widgets = (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload)
  aufgabeTemplate Testen (AufgabeTestenR server typ konfiguration) widgets mbewertung

postAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeTestenR server typ konfiguration = do
  let vorherigeEinsendung = Just $ Textarea "h (a (c , g (d , k )), l (m , f (i , b )))"
  ((result, formWidget), formEnctype) <- runFormPost $ aufgabeLösenForm vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost lösungHochladenForm
  let mbewertung = Just bew
      widgets = (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload)
  aufgabeTemplate Testen (AufgabeTestenR server typ konfiguration) widgets mbewertung
