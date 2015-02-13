module Handler.Aufgabe.Forms (Aktion (Anlegen, Bearbeiten, Entfernen), AufgabeForm (ServerForm, AufgabeTypForm, AufgabeForm, VorlagenForm, KonfigurationForm, HochladenForm, TestenForm), aktionName, getId, getTitel, getUnsafePostParams, readAktion, serverForm, typForm, aufgabeForm, vorlagenForm, konfigurationForm, hochladenForm, testenForm) where

import Import
import qualified Handler.Aufgabe as A
import qualified Handler.AufgabeEinstellungen as E (aufgabeForm)
import Handler.AufgabeEinstellungen (AufgabeFormDaten)
import qualified Handler.AufgabeKonfiguration as K (konfigurationForm)
import Handler.AufgabeVorlagen (getVorlagen)
import qualified Handler.Servers as S (serversForm)

import Yesod.Form.Fields.TreeValueField (treeValueField)

import Types.Basic (Task)
import Types.Instance (Instance (Instance))
import Types.Signed (Signed (Signed))
import Util.Hash (hash)

data AufgabeForm = ServerForm | AufgabeTypForm | AufgabeForm | VorlagenForm | KonfigurationForm | HochladenForm | TestenForm

aktionName :: Text
aktionName = "aktion"

data Aktion = Anlegen | Bearbeiten | Entfernen deriving (Show, Read)

readAktion :: Text -> Aktion
readAktion t = read $ unpack t

getId :: AufgabeForm -> Text
getId f =
   case f of
     ServerForm -> "server"
     AufgabeTypForm -> "typ"
     AufgabeForm -> "aufgabe"
     VorlagenForm -> "vorlage"
     KonfigurationForm -> "konfiguration"
     HochladenForm -> "hochladen"
     TestenForm -> "testen"

getTitel :: AufgabeForm -> AutotoolMessage
getTitel f =
  case f of
    ServerForm -> MsgServerWählen
    AufgabeTypForm -> MsgAufgabeTyp
    AufgabeForm -> MsgAufgabeEinstellungen
    VorlagenForm -> MsgAufgabeVorlagen
    KonfigurationForm -> MsgAufgabeKonfiguration
    HochladenForm -> MsgAufgabeHochladen
    TestenForm -> MsgAufgabeTesten

testenForm ::
    ToMarkup t =>
    t ->
    Maybe (Signed (Task, Instance)) ->
    ServerUrl ->
    AufgabeTyp ->
    Maybe VorlageName ->
    AufgabeKonfiguration ->
    AufgabeFormDaten ->
    Maybe Text ->
    Form (ServerUrl, AufgabeTyp, Maybe VorlageName, AufgabeKonfiguration, AufgabeFormDaten, Text)
testenForm atyp msigned server typ vorlage konfiguration einstellungen mlösung =
  let signed = maybe (Signed ("", Instance "" "") (hash ' ')) id msigned
  in identifyForm (getId TestenForm) $ renderBootstrap3 BootstrapBasicForm $ (,,,,,)
     <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
     <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
     <*> areq hiddenField (bfs $ pack $ show vorlage) {fsName = Just $ getId VorlagenForm} (Just vorlage)
     <*> areq hiddenField (bfs $ konfiguration) {fsName = Just $ getId KonfigurationForm} (Just konfiguration)
     <*> areq hiddenField (bfs $ pack $ show einstellungen) {fsName = Just $ getId AufgabeForm} (Just einstellungen)
     <*> A.aufgabeEinsendenForm (A.checkEinsendung server signed) atyp mlösung

hochladenForm :: ServerUrl -> AufgabeTyp -> Maybe VorlageName -> AufgabeKonfiguration -> AufgabeFormDaten -> Form (ServerUrl, AufgabeTyp, Maybe VorlageName, AufgabeKonfiguration, AufgabeFormDaten, FileInfo)
hochladenForm server typ vorlage konfiguration einstellungen =
  identifyForm (getId HochladenForm) $ renderBootstrap3 BootstrapBasicForm $ (,,,,,)
  <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
  <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
  <*> areq hiddenField (bfs $ pack $ show vorlage) {fsName = Just $ getId VorlagenForm} (Just vorlage)
  <*> areq hiddenField (bfs $ konfiguration) {fsName = Just $ getId KonfigurationForm} (Just konfiguration)
  <*> areq hiddenField (bfs $ pack $ show einstellungen) {fsName = Just $ getId AufgabeForm} (Just einstellungen)
  <*> A.einsendungHochladenForm

konfigurationForm :: ToMarkup t => t -> ServerUrl -> AufgabeTyp -> Maybe VorlageName -> Maybe AufgabeKonfiguration -> Form (ServerUrl, AufgabeTyp, Maybe VorlageName, AufgabeKonfiguration)
konfigurationForm ktyp server typ vorlage mkonfiguration =
  identifyForm (getId KonfigurationForm) $ renderBootstrap3 BootstrapBasicForm $ (,,,)
  <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
  <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
  <*> areq hiddenField (bfs $ pack $ show vorlage) {fsName = Just $ getId VorlagenForm} (Just vorlage)
  <*> K.konfigurationForm server typ ktyp mkonfiguration

vorlagenForm :: ServerUrl -> AufgabeTyp -> Maybe (Maybe VorlageName) -> Form (ServerUrl, AufgabeTyp, Maybe VorlageName)
vorlagenForm server typ mvorlage =
    identifyForm (getId VorlagenForm) $ renderBootstrap3 BootstrapInlineForm $ (,,)
    <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
    <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
    <*> areq (radioField options) (bfs MsgAufgabeVorlagen) mvorlage
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeVorlagen "btn-success" [])
  where
    options = do
      render <- getMessageRender
      vorlagen <- getVorlagen typ
      let standard = render MsgAufgabeVorlageStandard
          standard' = Option standard Nothing $ toPathPiece (Nothing :: Maybe VorlageName)
          os = fmap (\v -> Option v (Just v) $ toPathPiece $ Just v) vorlagen
      return $ mkOptionList $ standard' : os

aufgabeForm :: Either a b -> ServerUrl -> AufgabeTyp -> Maybe VorlageName -> AufgabeKonfiguration -> Maybe AufgabeFormDaten -> Form (AufgabeTyp, ServerUrl, Maybe VorlageName, AufgabeKonfiguration, AufgabeFormDaten)
aufgabeForm eid server typ vorlage konfiguration maufgabe =
  let aktion v = [("name", aktionName), ("value", pack $ show v)]
  in identifyForm (getId AufgabeForm) $ renderBootstrap3 BootstrapBasicForm $ (,,,,)
     <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
     <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
     <*> areq hiddenField (bfs $ pack $ show vorlage) {fsName = Just $ getId VorlagenForm} (Just vorlage)
     <*> areq hiddenField (bfs $ konfiguration) {fsName = Just $ getId KonfigurationForm} (Just konfiguration)
     <*> E.aufgabeForm typ maufgabe
     <* bootstrapSubmit (BootstrapSubmit (either (const MsgAufgabeAnlegen) (const MsgAufgabeBearbeiten) eid) "btn-success" $ aktion $ either (const Anlegen) (const Bearbeiten) eid)
     <* either (\_ -> pure ()) (\_ -> bootstrapSubmit (BootstrapSubmit MsgLöschen "btn-danger" $ aktion Entfernen)) eid

typForm :: Forest AufgabeTyp -> ServerUrl -> Maybe AufgabeTyp -> Form (AufgabeTyp, ServerUrl)
typForm forest server mtyp =
  identifyForm (getId AufgabeTypForm) $ renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
    <*> areq (treeValueField forest) (bfs MsgAufgabeTyp) mtyp
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeTyp "btn-success" [])

serverForm :: Maybe ServerUrl -> Form ServerUrl
serverForm mserver =
  identifyForm (getId ServerForm) $ S.serversForm mserver

-- | Liefert die Parameter der versteckten Eingabefelder, falls das Feld im POST nicht existierte, wird der Wert auf 'undefined' gesetzt. Die gelieferten Werte sind somit nur zum Weiterreichen bei der Formularauswertung brauchbar, jedoch nicht für das Rendern von Formulardaten oder verwenden in Berechnungen.
getUnsafePostParams :: RenderMessage Autotool FormMessage => Handler (ServerUrl, AufgabeTyp, VorlageName, AufgabeKonfiguration, AufgabeFormDaten)
getUnsafePostParams = do
  let auswerten a = case a of
                   FormSuccess a' -> a'
                   _ -> undefined
  s <- liftM auswerten $ runInputPostResult $ ireq hiddenField $ getId ServerForm
  t <- liftM auswerten $ runInputPostResult $ ireq hiddenField $ getId AufgabeTypForm
  v <- liftM auswerten $ runInputPostResult $ ireq hiddenField $ getId VorlagenForm
  k <- liftM auswerten $ runInputPostResult $ ireq hiddenField $ getId KonfigurationForm
  a <- liftM auswerten $ runInputPostResult $ ireq hiddenField $ getId AufgabeForm
  return (s, t, v, k, a)
