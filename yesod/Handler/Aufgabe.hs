module Handler.Aufgabe where

import Import
import Yesod.Form.Fields.PreField (preField)

import Service.Interface (grade_task_solution_localized)
import Types.Basic (Task)
import Types.Description (Description)
import Types.Documented (Documented)
import Types.Instance (Instance (Instance))
import Types.Signed (Signed (Signed))
import Types.Solution (Solution (SString))
import Util.Hash (hash)

hinweis :: String
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
      server = ""
      signed = Signed ("", Instance "" "") (hash ' ')
  ((result, formWidget), formEnctype) <- runFormPost $ identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $ aufgabeEinsendenForm (checkEinsendung server signed) typ vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm einsendungHochladenForm
  let widgets = (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload)
      mbewertung = Just bew
  aufgabeTemplate (AufgabeR aufgabe) widgets mbewertung

aufgabeTemplate :: Route Autotool -> (Widget, Enctype, Widget, Enctype) -> Maybe Text -> Handler Html 
aufgabeTemplate zielAdresse (formWidget, formEnctype, formWidgetUpload, formEnctypeUpload) mbewertung = do
  let name = "Reconstruct-Direct-1" :: Text
      titel = MsgAufgabeXTesten name
  mvorlageForm <- liftM Just $ generateFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm vorlageForm
  defaultLayout $ do
    $(widgetFile "aufgabe")

einsendungHochladenForm :: AForm Handler FileInfo
einsendungHochladenForm =
  areq fileField "Datei Hochladen" Nothing
  <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" [])

vorlageForm :: AForm Handler ()
vorlageForm =
  let actionType v = [("name", "action"), ("value", v)]
  in bootstrapSubmit (BootstrapSubmit MsgBeispielLaden "btn-primary" $ actionType "beispielLaden")
     *> bootstrapSubmit (BootstrapSubmit MsgVorherigeEinsendungLaden "btn-primary" $ actionType "vorherigeEinsendung")

einsendenId :: Text
einsendenId = "einsenden"

checkEinsendung :: ServerUrl -> Signed (Task, Instance) -> Text -> Handler (Either Description (Documented Double))
checkEinsendung server signedAufgabe einsendung = do
  sprache <- getBevorzugteSprache
  lift $ grade_task_solution_localized (unpack server) signedAufgabe (SString $ unpack einsendung) sprache

aufgabeEinsendenForm :: ToMarkup t => (Text -> Handler (Either a b)) -> t -> Maybe Text -> AForm Handler Text
aufgabeEinsendenForm checkMethode typ meinsendung =
    aopt (preField typ) (bfs MsgAufgabeLösungTyp) {fsAttrs = []} Nothing
     *> areq einsendenField (addAttrs $ bfs MsgLösung) meinsendung
     <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" [])
  where
    addAttrs field = field {
        fsAttrs = ("rows", pack . show $ 2 + maybe 0 (length . lines) meinsendung) : fsAttrs field,
        fsName = Just einsendenId
      }
    einsendenField = checkMMap (\ (Textarea einsendung) -> do
        check' <- checkMethode einsendung
        case check' of
          Left _ -> return $ Left MsgFehler
          Right _ -> return $ Right einsendung
      ) Textarea textareaField
