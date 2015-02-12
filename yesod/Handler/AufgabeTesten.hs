module Handler.AufgabeTesten where

import Import
import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html ((!))
import Util.Xml.Output (xmlStringToOutput)

import Handler.Aufgabe (aufgabeEinsendenForm, checkEinsendung, einsendenId, einsendungHochladenForm)
import Handler.AufgabeKonfiguration (checkKonfiguration)

import Autolib.Output (render)
import Autolib.Multilingual (specialize)
import Service.Interface (get_task_instance_localized)
import Types.Basic (Task)
import Types.Config (Config)
import Types.Description (Description (DString))
import Types.Documented (Documented (contents), documentation)
import Types.Instance (Instance)
import Types.Signed (Signed)
import Types.Solution (Solution (SString))

import Data.Conduit (($$))
import Data.Conduit.Binary (sinkLbs)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

getAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Text -> Handler Html
getAufgabeTestenR = postAufgabeTestenR

postAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Text -> Handler Html
postAufgabeTestenR server typ konfiguration benutzerId = do
  esigned <- checkKonfiguration server typ konfiguration
  signed <- case esigned of
    Left fehler -> do
      setMessage fehler
      redirect $ AufgabeKonfigurationR server typ konfiguration
    Right signed' -> return signed'
  (signed', vorherigeEinsendung, atyp, aufgabenstellung) <-
    getAufgabeInstanz server signed benutzerId
  (formWidget, formEnctype) <- generateFormPost $ identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $ aufgabeEinsendenForm (checkEinsendung server signed') atyp $ Just vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm $ einsendungHochladenForm
  let hinweis = "" :: Text
      zielAdresse = AufgabeTestenR server typ konfiguration benutzerId
      titel = MsgAufgabeXTesten typ
      mvorlageForm = Nothing :: Maybe (Widget, Enctype)
      mfile = case resultUpload of
                FormSuccess f -> Just f
                _ -> Nothing
  mbewertung <- getBewertung server signed' mfile
  defaultLayout $ do
    $(widgetFile "aufgabe")

getAufgabeInstanz :: ServerUrl -> Signed (Task, Config) -> Text -> Handler (Signed (Task, Instance), Text, Html, Html)
getAufgabeInstanz server signed benutzerId = do
  sprache <- getBevorzugteSprache
  (signed', DString aufgabenstellung, einsendung) <- lift $ get_task_instance_localized (unpack server) signed (unpack benutzerId) sprache
  let SString einsendung' = contents einsendung
      DString atyp = documentation einsendung
      atyp' = specialize sprache $ render $ xmlStringToOutput atyp
      aufgabenstellung' = specialize sprache $ render $ xmlStringToOutput aufgabenstellung
  return (signed', pack einsendung', atyp', aufgabenstellung')

getBewertung :: ServerUrl -> Signed (Task, Instance) -> Maybe FileInfo -> Handler (Maybe Html)
getBewertung server signed mfile = runMaybeT $ do
  sprache <- getBevorzugteSprache
  meinsendung <- case mfile of
    Just r -> liftM (Just . toStrict . decodeUtf8) $ lift $ fileSource r $$ sinkLbs
    Nothing -> lookupPostParam einsendenId
  einsendung <- MaybeT . return $ meinsendung
  bewertung <- lift $ checkEinsendung server signed einsendung
  let DString beschreibung = either id documentation bewertung
      hinweis' = specialize sprache $ render $ xmlStringToOutput beschreibung
  return $ hinweis' ! (class_ $ either (const "alert-danger") (const "alert-success") bewertung)
