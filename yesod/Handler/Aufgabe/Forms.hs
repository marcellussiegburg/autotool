module Handler.Aufgabe.Forms (AufgabeForm (ServerForm, AufgabeTypForm, AufgabeForm, VorlagenForm, KonfigurationForm, HochladenForm, TestenForm), getId, getTitel, serverForm, typForm, aufgabeForm, vorlagenForm, konfigurationForm, hochladenForm, testenForm) where

import Import
import qualified Handler.Aufgabe as A
import qualified Handler.AufgabeEinstellungen as E (aufgabeForm')
import Handler.AufgabeEinstellungen (AufgabeFormDaten)
import qualified Handler.AufgabeKonfiguration as K (konfigurationForm)

import Yesod.Form.Fields.TreeValueField (treeValueField)

data AufgabeForm = ServerForm | AufgabeTypForm | AufgabeForm | VorlagenForm | KonfigurationForm | HochladenForm | TestenForm

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

testenForm :: ToMarkup t => t -> ServerUrl -> AufgabeTyp -> AufgabeFormDaten -> Maybe VorlageName -> AufgabeKonfiguration -> Maybe Text -> Form (ServerUrl, AufgabeTyp, AufgabeFormDaten, Maybe VorlageName, AufgabeKonfiguration, Text)
testenForm atyp server typ einstellungen vorlage konfiguration mlösung =
  identifyForm (getId TestenForm) $ renderBootstrap3 BootstrapBasicForm $ (,,,,,)
  <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
  <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
  <*> areq hiddenField (bfs $ pack $ show einstellungen) {fsName = Just $ getId AufgabeForm} (Just einstellungen)
  <*> areq hiddenField (bfs $ pack $ show vorlage) {fsName = Just $ getId VorlagenForm} (Just vorlage)
  <*> areq hiddenField (bfs $ konfiguration) {fsName = Just $ getId KonfigurationForm} (Just konfiguration)
  <*> A.aufgabeLösenForm atyp mlösung

hochladenForm :: ServerUrl -> AufgabeTyp -> AufgabeFormDaten -> Maybe VorlageName -> AufgabeKonfiguration -> Form (ServerUrl, AufgabeTyp, AufgabeFormDaten, Maybe VorlageName, AufgabeKonfiguration, FileInfo)
hochladenForm server typ einstellungen vorlage konfiguration =
  identifyForm (getId HochladenForm) $ renderBootstrap3 BootstrapBasicForm $ (,,,,,)
  <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
  <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
  <*> areq hiddenField (bfs $ pack $ show einstellungen) {fsName = Just $ getId AufgabeForm} (Just einstellungen)
  <*> areq hiddenField (bfs $ pack $ show vorlage) {fsName = Just $ getId VorlagenForm} (Just vorlage)
  <*> areq hiddenField (bfs $ konfiguration) {fsName = Just $ getId KonfigurationForm} (Just konfiguration)
  <*> A.lösungHochladenForm

konfigurationForm :: ToMarkup t => t -> Either a b -> ServerUrl -> AufgabeTyp -> AufgabeFormDaten -> Maybe VorlageName -> Maybe AufgabeKonfiguration -> Form (ServerUrl, AufgabeTyp, AufgabeFormDaten, Maybe VorlageName, AufgabeKonfiguration)
konfigurationForm ktyp eid server typ einstellungen vorlage mkonfiguration =
  identifyForm (getId KonfigurationForm) $ renderBootstrap3 BootstrapBasicForm $ (,,,,)
  <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
  <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
  <*> areq hiddenField (bfs $ pack $ show einstellungen) {fsName = Just $ getId AufgabeForm} (Just einstellungen)
  <*> areq hiddenField (bfs $ pack $ show vorlage) {fsName = Just $ getId VorlagenForm} (Just vorlage)
  <*> K.konfigurationForm ktyp mkonfiguration
  <* bootstrapSubmit (BootstrapSubmit (either (\_ -> MsgAufgabeAnlegen) (\_ -> MsgAufgabeBearbeiten) eid) "btn-success" [])
  <* either (\_ -> pure ()) (\_ -> bootstrapSubmit (BootstrapSubmit MsgLöschen "btn-danger" [])) eid

vorlagenForm :: [Text] -> ServerUrl -> AufgabeTyp -> AufgabeFormDaten -> Maybe (Maybe VorlageName) -> Form (ServerUrl, AufgabeTyp, AufgabeFormDaten, Maybe VorlageName)
vorlagenForm vorlagen server typ einstellungen mvorlage =
  identifyForm (getId VorlagenForm) $ renderBootstrap3 BootstrapInlineForm $ (,,,)
  <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
  <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
  <*> areq hiddenField (bfs $ pack $ show einstellungen) {fsName = Just $ getId AufgabeForm} (Just einstellungen)
  <*> areq (radioFieldList $ ("Standardvorlage / letzte Konfiguration", Nothing) : fmap (\a -> (a, Just a)) vorlagen) (bfs MsgServer) mvorlage
 <* bootstrapSubmit (BootstrapSubmit MsgServerWählen "btn-success" [])

aufgabeForm :: ServerUrl -> AufgabeTyp -> Maybe AufgabeFormDaten -> Form (AufgabeTyp, ServerUrl, AufgabeFormDaten)
aufgabeForm server typ maufgabe =
  identifyForm (getId AufgabeForm) $ renderBootstrap3 BootstrapBasicForm $ (,,)
  <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
  <*> areq hiddenField (bfs $ typ) {fsName = Just $ getId AufgabeTypForm} (Just typ)
  <*> E.aufgabeForm' maufgabe

typForm :: Forest AufgabeTyp -> ServerUrl -> Maybe AufgabeTyp -> Form (AufgabeTyp, ServerUrl)
typForm forest server mtyp =
  identifyForm (getId AufgabeTypForm) $ renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq hiddenField (bfs $ server) {fsName = Just $ getId ServerForm} (Just server)
    <*> areq (treeValueField forest) (bfs MsgServer) mtyp
    <* bootstrapSubmit (BootstrapSubmit MsgServerWählen "btn-success" [])

serverForm :: Maybe ServerUrl -> Form ServerUrl
serverForm mserver =
  identifyForm (getId ServerForm) $ renderBootstrap3 BootstrapBasicForm $
    areq textField (withAutofocus $ bfs MsgServer) mserver
    <* bootstrapSubmit (BootstrapSubmit MsgServerWählen "btn-success" [])
