{-# LANGUAGE OverloadedStrings #-}
module Handler.Aufgabe where

import Import
import Yesod.Form.Fields.PreField (preField)

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
getAufgabeR = postAufgabeR

postAufgabeR :: AufgabeId -> Handler Html
postAufgabeR aufgabe = do
  let vorherigeEinsendung = Just $ "h (a (c , g (d , k )), l (m , f (i , b )))" :: Maybe Text
      typ = preEscapedToHtml ("<td><a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Raw.html#t:Term\"><tt>Term</tt></a>(<a href=\"http://hackage.haskell.org/package/ghc-prim/docs/GHC-Tuple.html#t:()\")><tt>()</tt></a>)(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>)</td>" :: Text)
  ((result, formWidget), formEnctype) <- runFormPost $ identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $ aufgabeLösenForm typ vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm $ lösungHochladenForm
  let mbewertung = Just bew
      widgets = (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload)
  aufgabeTemplate Lösen (AufgabeR aufgabe) widgets mbewertung

aufgabeTemplate :: Lösen -> Route Autotool -> (Widget, Enctype, Widget, Enctype) -> Maybe Text -> Handler Html 
aufgabeTemplate lösen zielAdresse (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload) mbewertung = do
  let name = "Reconstruct-Direct-1" :: Text
  defaultLayout $ do
    $(widgetFile "aufgabe")

lösungHochladenForm :: AForm Handler FileInfo
lösungHochladenForm = do
    areq fileField "Datei Hochladen" Nothing
    <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" [])

aufgabeLösenForm :: ToMarkup t => t -> Maybe Text -> AForm Handler Text
aufgabeLösenForm typ meinsendung =
  let actionType v = [("name", "action"), ("value", v)]
  in bootstrapSubmit (BootstrapSubmit MsgBeispielLaden "btn-primary" $ actionType "beispielLaden")
     *> bootstrapSubmit (BootstrapSubmit MsgVorherigeEinsendungLaden "btn-primary" $ actionType "vorherigeEinsendung")
     *> aopt (preField typ) (bfs MsgAufgabeLösungTyp) {fsAttrs = []} Nothing
     *> (unTextarea <$> areq textareaField (bfs MsgLösung) (fmap Textarea meinsendung))
     <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" $ actionType "Lösung absenden")
