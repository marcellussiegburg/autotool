module Handler.AufgabeTesten where

import Import

import Handler.Aufgabe (aufgabeEinsendenForm, checkEinsendung, getAufgabeInstanz, getBewertung, getCrc, einsendungHochladenForm)
import Handler.AufgabeKonfiguration (checkKonfiguration)
import Control.Types (VNr (VNr), MNr (MNr))

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
    getAufgabeInstanz server signed $ getCrc (VNr 42) Nothing $ MNr $ unpack benutzerId
  (formWidget, formEnctype) <- generateFormPost $ identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $ aufgabeEinsendenForm (checkEinsendung server signed') atyp $ Just vorherigeEinsendung
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm $ einsendungHochladenForm
  let hinweis = "" :: Text
      zielAdresse = AufgabeTestenR server typ konfiguration benutzerId
      titel = MsgAufgabeXTesten typ
      mfile = case resultUpload of
                FormSuccess f -> Just f
                _ -> Nothing
      mvorlageForm = Nothing :: Maybe (Widget, Enctype)
      mlog = Nothing :: Maybe (Text)
  mbewertung <- liftM (fmap snd) $ getBewertung server signed' mfile
  defaultLayout $ do
    $(widgetFile "aufgabe")
