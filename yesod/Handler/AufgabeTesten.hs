module Handler.AufgabeTesten where

import Import

import Handler.Aufgabe

getAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeTestenR = postAufgabeTestenR

postAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeTestenR server typ konfiguration = do
  let vorherigeEinsendung = Just $ "h (a (c , g (d , k )), l (m , f (i , b )))"
      atyp = preEscapedToHtml ("<td><a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Raw.html#t:Term\"><tt>Term</tt></a>(<a href=\"http://hackage.haskell.org/package/ghc-prim/docs/GHC-Tuple.html#t:()\")><tt>()</tt></a>)(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>)</td>" :: Text)
  ((result, formWidget), formEnctype) <- runFormPost $ identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $ aufgabeLösenForm atyp vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm $ lösungHochladenForm
  let mbewertung = Just bew
      widgets = (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload)
  aufgabeTemplate Testen (AufgabeTestenR server typ konfiguration) widgets mbewertung
