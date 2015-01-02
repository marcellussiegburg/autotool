{-# LANGUAGE OverloadedStrings #-}
module Handler.Aufgabe where

import Import

data Lösen = Lösen | Testen

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

bew :: Text
bew = "gelesen: h (a (c , g (d , k )), l (m , f (i , b )))\n\
partiell korrekt?\n\
Ihr Baum t ist h (a (c , g (d , k )), l (m , f (i , b )))\n\
               \n\
                   h            \n\
                   |            \n\
                   +------\\     \n\
                   |      |     \n\
                   a      l     \n\
                   |      |     \n\
                   +-g    +-f   \n\
                   | |    | |   \n\
                   | +-k  | +-b \n\
                   | |    | |   \n\
                   | `-d  | `-i \n\
                   |      |     \n\
                   `-c    `-m   \n\
total korrekt?\n\
Preorder () =\n\
    Preorder (Eingabe) = [ h , a\n\
                         , c , g , d , k , l , m , f , i\n\
                         , b ]\n\
    Preorder (Gesucht) = [ e , j , b\n\
                         , i , f , m , l , k , d , g , c\n\
                         , a , h ]\n\
    stimmen überein? False\n\
Inorder () =\n\
    Inorder (Eingabe) = [ c , a\n\
                        , d , g , k , h , m , l , i , f\n\
                        , b ]\n\
    Inorder (Gesucht) = [ b , j , i\n\
                        , e , k , l , d , m , g , f , a\n\
                        , c , h ]\n\
    stimmen überein? False\n\
stimmt alles überein?\n\
Nein.\n\
Bewertung der Einsendung: No"

getAufgabeR :: AufgabeId -> Handler Html
getAufgabeR aufgabe = do
    let vorherigeEinsendung = Just $ Textarea "h (a (c , g (d , k )), l (m , f (i , b )))"
    (formWidget, formEnctype) <- generateFormPost $ aufgabeLösenForm vorherigeEinsendung
    (formWidgetUpload, formEnctypeUpload) <- generateFormPost lösungHochladenForm
    let mbewertung = Nothing
        widgets = (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload)
    aufgabeTemplate Lösen (AufgabeR aufgabe) widgets mbewertung

postAufgabeR :: AufgabeId -> Handler Html
postAufgabeR aufgabe = do
  let vorherigeEinsendung = Just $ Textarea "h (a (c , g (d , k )), l (m , f (i , b )))"
  ((result, formWidget), formEnctype) <- runFormPost $ aufgabeLösenForm vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost lösungHochladenForm
  let mbewertung = Just bew
      widgets = (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload)
  aufgabeTemplate Lösen (AufgabeR aufgabe) widgets mbewertung

aufgabeTemplate :: Lösen -> Route Autotool -> (Widget, Enctype, Widget, Enctype) -> Maybe Text -> Handler Html 
aufgabeTemplate lösen zielAdresse (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload) mbewertung = do
  let typ = preEscapedToHtml ("<td><a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Raw.html#t:Term\"><tt>Term</tt></a>(<a href=\"http://hackage.haskell.org/package/ghc-prim/docs/GHC-Tuple.html#t:()\")><tt>()</tt></a>)(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>)</td>" :: Text)
      name = "Reconstruct-Direct-1" :: Text
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
