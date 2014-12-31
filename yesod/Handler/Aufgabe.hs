{-# LANGUAGE OverloadedStrings #-}
module Handler.Aufgabe where

import Import

hinweis :: Text
hinweis = "nock kein Hinweis"

aufgabenstellung :: Text
aufgabenstellung = "Gesucht ist ein binärer Baum t mit den Knoten-Reihenfolgen:\n\
   Preorder (t) = [ e , j , b\n\
                  , i , f , m , l , k , d , g , c\n\
                  , a , h ]\n\
   Inorder (t) = [ b , j , i , e\n\
                 , k , l , d , m , g , f , a , c\n\
                 , h ]"

getAufgabeR :: AufgabeId -> Handler Html
getAufgabeR aufgabe = do
  let typ = preEscapedToHtml ("<td><a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Raw.html#t:Term\"><tt>Term</tt></a>(<a href=\"http://hackage.haskell.org/package/ghc-prim/docs/GHC-Tuple.html#t:()\")><tt>()</tt></a>)(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>)</td>" :: Text)
      name = "Reconstruct-Direct-1" :: Text
      vorherigeEinsendung = Just $ Textarea "h (a (c , g (d , k )), l (m , f (i , b )))"
  (formWidget, formEnctype) <- generateFormPost $ aufgabeLösenForm vorherigeEinsendung
  (formWidgetUpload, formEnctypeUpload) <- generateFormPost lösungHochladenForm
  defaultLayout $ do
    $(widgetFile "aufgabe")

postAufgabeR :: AufgabeId -> Handler Html
postAufgabeR aufgabe = do
  let typ = preEscapedToHtml ("<td><a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Raw.html#t:Term\"><tt>Term</tt></a>(<a href=\"http://hackage.haskell.org/package/ghc-prim/docs/GHC-Tuple.html#t:()\")><tt>()</tt></a>)(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>)</td>" :: Text)
      name = "Reconstruct-Direct-1" :: Text
      vorherigeEinsendung = Just $ Textarea "h (a (c , g (d , k )), l (m , f (i , b )))"
  ((result, formWidget), formEnctype) <- runFormPost $ aufgabeLösenForm vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost lösungHochladenForm
  defaultLayout $ do
    $(widgetFile "aufgabe")

lösungHochladenForm :: Form FileInfo
lösungHochladenForm = do
  identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm $
    areq fileField "Datei Hochladen" Nothing
    <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" [])

aufgabeLösenForm :: Maybe Textarea -> Form Textarea
aufgabeLösenForm meinsendung = do
  let actionType v = [("name", "action"), ("value", v)]
  identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $
    bootstrapSubmit (BootstrapSubmit MsgBeispielLaden "btn-primary" $ actionType "beispielLaden")
    *> bootstrapSubmit (BootstrapSubmit MsgVorherigeEinsendungLaden "btn-primary" $ actionType "vorherigeEinsendung")
    *> areq textareaField (bfs MsgLösung) meinsendung
    <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" $ actionType "Lösung absenden")
