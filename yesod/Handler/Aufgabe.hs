{-# LANGUAGE OverloadedStrings #-}
module Handler.Aufgabe where

import Import

getAufgabeR :: AufgabeId -> Handler Html
getAufgabeR aufgabe = do
  (formWidget, formEnctype) <- generateFormPost aufgabeLösenForm
  (formWidgetUpload, formEnctypeUpload) <- generateFormPost lösungHochladenForm
  defaultLayout $ do
    $(widgetFile "aufgabe")

postAufgabeR :: AufgabeId -> Handler Html
postAufgabeR aufgabe = do
  ((result, formWidget), formEnctype) <- runFormPost aufgabeLösenForm
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost lösungHochladenForm
  defaultLayout $ do
    $(widgetFile "aufgabe")

lösungHochladenForm :: Form FileInfo
lösungHochladenForm = do
  identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm $
    areq fileField "Datei Hochladen" Nothing
    <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" [])

aufgabeLösenForm :: Form Textarea
aufgabeLösenForm = do
  let actionType v = [("name", "action"), ("value", v)]
  identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $
    bootstrapSubmit (BootstrapSubmit MsgBeispielLaden "btn-primary" $ actionType "beispielLaden")
    *> bootstrapSubmit (BootstrapSubmit MsgVorherigeEinsendungLaden "btn-primary" $ actionType "vorherigeEinsendung")
    *> areq textareaField "Lösung" (Just $ Textarea "h (a (c , g (d , k )), l (m , f (i , b )))")
    <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" $ actionType "Lösung absenden")
