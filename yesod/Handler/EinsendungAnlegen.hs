module Handler.EinsendungAnlegen where

import Import
import Handler.AufgabeKonfiguration (checkKonfiguration)
import Yesod.Form.Fields.PreField (preField)

import qualified Control.Exception as Exception
import Data.ByteString (ByteString)
import Data.Conduit (($$))
import Data.Conduit.Binary (sinkLbs)
import Data.Digest.CRC32 (crc32)
import Data.String (fromString)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html ((!))
import Data.Time.Clock (getCurrentTime)
import Util.Xml.Output (xmlStringToOutput)

import Autolib.Output (render)
import Autolib.Multilingual (specialize)
import qualified Control.Student.DB as StudentDB (get_snr)
import qualified Control.Student.Type as Student
import qualified Control.Types as T
import Operate.Bank (bank)
import Operate.Click (Click (Example))
import qualified Operate.Param as P
import qualified Operate.Store as Store (Type (Input), latest)
import Service.Interface (get_task_instance_localized, grade_task_solution_localized)
import Types.Basic (Task)
import Types.Config (Config)
import Types.Description (Description (DString))
import Types.Documented (Documented (contents), documentation)
import Types.Instance (Instance)
import Types.Signed (Signed)
import Types.Solution (Solution (SString))

data Aktion = BeispielLaden | VorherigeEinsendungLaden deriving (Show, Read)

aktion :: Text
aktion = "aktion"

eingeben :: Text
eingeben = "eingeben"

hochladen :: Text
hochladen = "hochladen"

getEinsendungAnlegenR :: AufgabeId -> Handler Html
getEinsendungAnlegenR = postEinsendungAnlegenR

postEinsendungAnlegenR :: AufgabeId -> Handler Html
postEinsendungAnlegenR aufgabeId = do
  aktuelleZeit <- lift getCurrentTime
  maufgabe <- runDB $ get aufgabeId
  aufgabe <- case maufgabe of
    Nothing -> notFound
    Just a -> return a
  let server = aufgabeServer aufgabe
      typ = aufgabeTyp aufgabe
      konfiguration = aufgabeKonfiguration aufgabe
      mhinweis = aufgabeHinweis aufgabe
  studentId <- requireAuthId
  mstudent <- lift $ liftM listToMaybe $ StudentDB.get_snr $ T.SNr studentId
  student <- case mstudent of
    Nothing ->
      -- Benutzer nicht in DB gefunden (passiert nie)
      redirect $ AufgabeTestenR server typ konfiguration ""
    Just s -> return s
  case zeitStatus (aufgabeVon aufgabe) (aufgabeBis aufgabe) aktuelleZeit of
    T.Late -> do
      setMessageI MsgAufgabeVorbei
      redirect $ AufgabeTestenR server typ konfiguration $ pack . T.toString . Student.mnr $ student
    T.Early -> return ()
    T.Current -> return ()
  esigned <- checkKonfiguration server typ konfiguration
  signed <- case esigned of
    Left fehler -> do
      setMessage fehler
      redirect $ AufgabenR $ aufgabeVorlesungId aufgabe
    Right signed' -> return signed'
  ((resultUpload, formWidgetUpload), formEnctypeUpload) <- runFormPost $ identifyForm "hochladen" $ renderBootstrap3 BootstrapBasicForm einsendungHochladenForm
  let mfile = case resultUpload of
        FormSuccess f -> Just f
        _ -> Nothing
  mvorherigeEinsendung <- runMaybeT $ do
    maktion <- lookupPostParam $ pack $ show aktion
    let vorher = do
          mvorherigeEinsendung' <- lift $ getVorherigeEinsendung mstudent aufgabe aufgabeId
          maktuelleEinsendung <- lift $ getMaybeEinsendung mfile
          MaybeT . return $ maybe mvorherigeEinsendung' Just maktuelleEinsendung
    case maktion of
      Nothing -> vorher
      Just aktion' ->
        case read $ unpack aktion' of
          VorherigeEinsendungLaden -> vorher
          BeispielLaden -> MaybeT . return $ Nothing
  (signed', beispiel, atyp, aufgabenstellung) <-
    getAufgabeInstanz server signed $ getCrc (T.VNr $ keyToInt $ aufgabeVorlesungId aufgabe) (Just $ T.ANr $ keyToInt aufgabeId) (Student.mnr student)
  (formWidget, formEnctype) <- generateFormPost $ identifyForm "senden" $ renderBootstrap3 BootstrapBasicForm $ aufgabeEinsendenForm (checkEinsendung server signed') atyp $ Just $ fromMaybe beispiel mvorherigeEinsendung
  let hochladenForm = formToWidget (EinsendungAnlegenR aufgabeId) $ Just hochladen
      eingebenForm = formToWidget (EinsendungAnlegenR aufgabeId) $ Just eingeben
  mbewertung' <- getBewertung server signed' mfile
  let mbewertung = fmap snd mbewertung'
  mvorlageForm <- liftM Just $ generateFormPost $ identifyForm (pack $ show aktion) $ renderBootstrap3 BootstrapBasicForm vorlageForm
  mlog <- logSchreiben mstudent aufgabe aufgabeId aufgabenstellung mbewertung' mfile
  defaultLayout $ do
    $(widgetFile "einsendungAnlegen")

einsendungHochladenForm :: AForm Handler FileInfo
einsendungHochladenForm =
  areq fileField "Datei Hochladen" Nothing
  <* bootstrapSubmit (BootstrapSubmit MsgLösungAbsenden "btn-success" [])

vorlageForm :: AForm Handler ()
vorlageForm =
  let actionType v = [("name", pack $ show aktion), ("value", pack $ show v)]
  in bootstrapSubmit (BootstrapSubmit MsgBeispielLaden "btn-primary" $ actionType BeispielLaden)
     *> bootstrapSubmit (BootstrapSubmit MsgVorherigeEinsendungLaden "btn-primary" $ actionType VorherigeEinsendungLaden)

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
          Left _ -> return $ Left MsgEinsendungFalsch
          Right _ -> return $ Right einsendung
      ) Textarea textareaField

getCrc :: T.VNr -> Maybe T.ANr -> T.MNr -> Integer
getCrc vnr manr matrikel = fromIntegral $ crc32 (fromString (show vnr ++ show manr ++ T.toString matrikel) :: ByteString)

getAufgabeInstanz :: ServerUrl -> Signed (Task, Config) -> Integer -> Handler (Signed (Task, Instance), Text, Html, Html)
getAufgabeInstanz server signed crc = do
  sprache <- getBevorzugteSprache
  (signed', DString aufgabenstellung, einsendung) <- lift $ get_task_instance_localized (unpack server) signed (show crc) sprache
  let SString einsendung' = contents einsendung
      DString atyp = documentation einsendung
      atyp' = specialize sprache $ render $ xmlStringToOutput atyp
      aufgabenstellung' = specialize sprache $ render $ xmlStringToOutput aufgabenstellung
  return (signed', pack einsendung', atyp', aufgabenstellung')

getMaybeEinsendung :: Maybe FileInfo -> Handler (Maybe Text)
getMaybeEinsendung mfile = runMaybeT $ do
  meinsendung <- case mfile of
    Just r -> lift $ liftM (Just . toStrict . decodeUtf8) $ fileSource r $$ sinkLbs
    Nothing -> lookupPostParam einsendenId
  MaybeT . return $ meinsendung

getBewertung :: ServerUrl -> Signed (Task, Instance) -> Maybe FileInfo -> Handler (Maybe (Maybe Integer, Html))
getBewertung server signed mfile = runMaybeT $ do
  sprache <- getBevorzugteSprache
  meinsendung <- lift $ getMaybeEinsendung mfile
  einsendung <- MaybeT . return $ meinsendung
  bewertung <- lift $ checkEinsendung server signed einsendung
  let DString beschreibung = either id documentation bewertung
      hinweis' = specialize sprache $ render $ xmlStringToOutput beschreibung
      hinweis'' = hinweis' ! (class_ $ either (const "alert-danger") (const "alert-success") bewertung)
      mwert = either (const Nothing) (Just . round . contents) bewertung
  return (mwert, hinweis'')

getVorherigeEinsendung :: Maybe Student.Student -> Aufgabe -> AufgabeId -> Handler (Maybe Text)
getVorherigeEinsendung mstudent aufgabe aufgabeId = runMaybeT $ do
  let stringToMaybeText t = if null t then Nothing else Just $ pack t
  student <- MaybeT . return $ mstudent
  mvorherigeEinsendung <- lift $ lift $ Store.latest Store.Input (getDefaultParam student aufgabe aufgabeId)
    `Exception.catch` \ (Exception.SomeException _) -> return []
  MaybeT . return $ stringToMaybeText mvorherigeEinsendung

logSchreiben :: Maybe Student.Student -> Aufgabe -> AufgabeId -> Html -> Maybe (Maybe Integer, Html) -> Maybe FileInfo -> Handler (Maybe Text)
logSchreiben mstudent aufgabe aufgabeId aufgabenstellung mbewertung mfile = runMaybeT $ do
  bewertung <- MaybeT . return $ mbewertung
  student <- MaybeT . return $ mstudent
  meinsendung <- lift $ getMaybeEinsendung mfile
  einsendung <- MaybeT . return $ meinsendung
  lift $ lift $ liftM pack $ bank (getDefaultParam student aufgabe aufgabeId) {
      P.minstant = Just aufgabenstellung,
      P.input = Just $ unpack einsendung,
      P.report = Just $ snd bewertung,
      P.mresult = Just $ maybe T.No T.ok $ fst bewertung
    }

getDefaultParam :: Student.Student -> Aufgabe -> AufgabeId -> P.Type
getDefaultParam student aufgabe aufgabeId =
  P.Param { P.makers = [],
    P.mmatrikel = Just $ Student.mnr student,
    P.ident = Student.snr student,
    P.mpasswort = Nothing,

    P.aufgabe = T.Name $ unpack $ aufgabeName aufgabe,
    P.typ = T.Typ $ unpack $ aufgabeTyp aufgabe,
    P.anr = T.ANr $ keyToInt aufgabeId,
    P.vnr = T.VNr $ keyToInt $ aufgabeVorlesungId aufgabe,
    P.highscore = aufgabeHighscore aufgabe,

    P.minstant = Nothing,
    P.input = Nothing,
    P.report = Nothing,
    P.mresult = Nothing,

    P.wahl = "",
    P.click = Example,
    P.input_width = 80,
    P.conf =  error "Param.empty.conf",
    P.remark = error "Param.empty.remark",

    P.variante = error "Param.empty.variante",
    P.names = []
  }
