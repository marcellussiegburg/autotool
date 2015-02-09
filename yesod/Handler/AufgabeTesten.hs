module Handler.AufgabeTesten where

import Import
import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html ((!))
import Util.Xml.Output (xmlStringToOutput)

import Handler.Aufgabe (aufgabeEinsendenForm, checkEinsendung, einsendenId, einsendungHochladenForm)
import Autolib.Output (render)
import Autolib.Multilingual (specialize)
import Operate.Types (descr)
import Service.Interface (get_task_instance_localized, verify_task_config_localized)
import Types.Config (Config (CString))
import Types.Description (Description (DString))
import Types.Documented (Documented (contents), documentation)
import Types.Solution (Solution (SString))

import Data.Conduit (($$))
import Data.Conduit.Binary (sinkLbs)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

getAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeTestenR = postAufgabeTestenR

postAufgabeTestenR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeTestenR server typ konfiguration = do
  sprache <- getBevorzugteSprache
  esigned <- lift $ verify_task_config_localized (unpack server) (unpack typ) (CString $ unpack konfiguration) sprache
  signed <- case esigned of
    Left err -> do
      setMessage $ specialize sprache $ render $ descr err
      redirect $ AufgabeKonfigurationR server typ konfiguration
    Right signed' -> return signed'
  let seed = "1111"
  (signed', DString aufgabenstellung', einsendung) <- lift $ get_task_instance_localized (unpack server) signed seed sprache
  let SString einsendung' = contents einsendung
      DString atyp' = documentation einsendung
      vorherigeEinsendung = Just $ pack einsendung'
      atyp = specialize sprache $ render $ xmlStringToOutput atyp' :: Html
      aufgabenstellung = specialize sprache $ render $ xmlStringToOutput aufgabenstellung' :: Html
  (formWidget, formEnctype) <- generateFormPost $ identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $ aufgabeEinsendenForm (checkEinsendung server signed') atyp vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm $ einsendungHochladenForm
  let hinweis = "" :: Text
      zielAdresse = AufgabeTestenR server typ konfiguration
      name = typ
      titel = MsgAufgabeXTesten name
      mvorlageForm = Nothing :: Maybe (Widget, Enctype)
  mbewertung <- runMaybeT $ do
    meinsendung'' <- case resultUpload of
      FormSuccess r -> liftM (Just . toStrict . decodeUtf8) $ lift $ fileSource r $$ sinkLbs
      _ -> lookupPostParam einsendenId
    einsendung'' <- MaybeT . return $ meinsendung''
    bewertung <- lift $ checkEinsendung server signed' einsendung''
    let fromDString d = let DString s = d in s
        beschreibung = fromDString $ either id documentation bewertung
        hinweis' = specialize sprache $ render $ xmlStringToOutput beschreibung :: Html
    return $ hinweis' ! (class_ $ either (const "alert-danger") (const "alert-success") bewertung)
  defaultLayout $ do
    $(widgetFile "aufgabe")
