module Handler.AufgabeAnlegen where

import Import

import Data.List (head, tail)
import Data.Tuple6

import Handler.Aufgabe.Forms
import qualified Handler.AufgabeEinstellungen as AE (AufgabeFormDaten (..), aufgabeToFormDaten, formDatenToHiLo, formDatenToStatus)
import Handler.AufgabeVorlage (getVorlageKonfiguration)
import Handler.AufgabeKonfiguration (checkKonfiguration, getBeispielKonfiguration, getKonfigurationFehler)
import Handler.EinsendungAnlegen (getAufgabeInstanz, getBewertung, getCrc)

import qualified Control.Aufgabe.DB as AufgabeDB
import qualified Control.Aufgabe.Typ as A (Aufgabe (..))
import Control.Types
import Service.Interface (get_task_types)
import Types.Basic (Task)
import Types.Instance (Instance)
import Types.Signed (Signed (signature))

data AutotoolForm = AutotoolForm {
  titel :: AutotoolMessage,
  name :: Text,
  enctype :: Enctype,
  route :: Route Autotool,
  widget :: Widget,
  attributes :: [(Text, Text)]
}

type FormResults =
    (Maybe ServerUrl,
     Maybe AufgabeTyp,
     Maybe (Maybe VorlageName),
     Maybe AufgabeKonfiguration,
     Maybe AE.AufgabeFormDaten,
     Maybe Text,
     Maybe FileInfo)

getAufgabeAnlegenR :: VorlesungId -> Handler Html
getAufgabeAnlegenR = postAufgabeAnlegenR

postAufgabeAnlegenR :: VorlesungId -> Handler Html
postAufgabeAnlegenR vorlesung = do
  aufgabeTemplate (Left vorlesung)

aufgabeTemplate :: Either VorlesungId A.Aufgabe -> Handler Html
aufgabeTemplate eidAufgabe = do
  let mhinweis = Nothing :: Maybe Text
  results <- getFormResults eidAufgabe
  let (mserver, mtyp, mvorlage, mkonfiguration, _, _, mhochladen) = results
  maktion <- liftM (fmap readAktion) $ lookupPostParam aktionName
  msignedK <- liftM (>>= rightToMaybe) $
    sequence $ checkKonfiguration <$> mserver <*> mtyp <*> mkonfiguration
  _ <- runMaybeT $ do
    aktion <- MaybeT . return $ maktion
    case aktion of
      Anlegen -> do
        vorlesungId <- MaybeT . return $ leftToMaybe eidAufgabe
        aufgabe' <- MaybeT . return $ aufgabeBearbeiten results (fromCGI . signature <$> msignedK) Nothing
        let aufgabe'' = aufgabe' { A.vnr = VNr $ keyToInt vorlesungId }
        lift $ lift $ AufgabeDB.put Nothing aufgabe''
        lift $ setMessageI MsgAufgabeAngelegt
        redirect $ AufgabenR vorlesungId -- TODO: redirect $ AufgabeR aufgabeId
      Bearbeiten -> do
        aufgabe <- MaybeT . return $ rightToMaybe eidAufgabe
        aufgabe' <- MaybeT . return $ aufgabeBearbeiten results (fromCGI . signature <$> msignedK) $ Just aufgabe
        lift $ lift $ AufgabeDB.put (Just $ A.anr aufgabe) aufgabe'
        lift $ setMessageI MsgAufgabeBearbeitet
      Entfernen -> do
        aufgabe <- MaybeT . return $ rightToMaybe eidAufgabe
        lift $ lift $ AufgabeDB.delete $ A.anr aufgabe
        lift $ setMessageI MsgAufgabeEntfernt
        let VNr vorlesungId = A.vnr aufgabe
        redirect $ AufgabenR $ intToKey vorlesungId
  (beispielKonfiguration, ktyp) <- maybe (return ("","")) id $
    getBeispielKonfiguration <$> mserver <*> mtyp
  mvorlageKonfiguration <- sequence $ getVorlageKonfiguration <$> Just beispielKonfiguration <*> mtyp <*> (maybe "" id <$> mvorlage)
  mhinweisFehler <- liftM join $ sequence $ getKonfigurationFehler <$> mserver <*> mtyp <*> maybe mvorlageKonfiguration Just mkonfiguration
  let matrikel = MNr "11111" -- TODO auswählbar machen
      crc = case eidAufgabe of
        Left v -> getCrc (VNr $ keyToInt v) Nothing matrikel
        Right aufgabe -> getCrc (A.vnr aufgabe) (Just $ A.anr aufgabe) matrikel
  (msignedA, meinsendung, atyp, maufgabenstellung) <-
    liftM (maybe (Nothing, Nothing, "" :: Html, Nothing)
        (\ (a, b, c, d) -> (Just a, Just b, c, Just d))) $
    sequence $ getAufgabeInstanz <$> mserver <*> msignedK <*> Just crc
  mbewertung <- liftM (fmap snd . join) $ sequence $ getBewertung <$> mserver <*> msignedA <*> Just mhochladen
  forms <- getForms results eidAufgabe ktyp msignedA atyp mvorlageKonfiguration meinsendung
  defaultLayout $ do
    addStylesheet $ StaticR css_tree_css
    $(widgetFile "aufgabeAnlegen")

aufgabeBearbeiten :: FormResults -> Maybe Signature -> Maybe A.Aufgabe -> Maybe A.Aufgabe
aufgabeBearbeiten results msignatur maufgabe = do
  let (mserver, mtyp, _, mkonfiguration, meinstellungen, _, _) = results
  server        <- mserver
  typ           <- mtyp
  konfiguration <- mkonfiguration
  einstellungen <- meinstellungen
  signatur      <- msignatur
  return $ A.Aufgabe {
    A.anr = maybe undefined A.anr maufgabe,
    A.vnr = maybe undefined A.vnr maufgabe,
    A.name = Name . unpack $ AE.name einstellungen,
    A.server = fromCGI $ unpack server,
    A.typ = fromCGI $ unpack typ,
    A.config = fromCGI $ unpack konfiguration,
    A.signature = signatur,
    A.remark = fromCGI $ unpack $ maybe "" id $ AE.hinweis einstellungen,
    A.highscore = AE.formDatenToHiLo $ AE.highscore einstellungen,
    A.status = AE.formDatenToStatus $ AE.status einstellungen,
    A.von = dayTimeToTime (AE.beginn einstellungen) $ AE.beginnZeit einstellungen,
    A.bis = dayTimeToTime (AE.ende einstellungen) $ AE.endeZeit einstellungen,
    A.timeStatus = undefined
  }

getForms :: FormResults -> Either VorlesungId A.Aufgabe -> Html -> Maybe (Signed (Task, Instance)) -> Html -> Maybe AufgabeKonfiguration -> Maybe Text -> Handler [AutotoolForm]
getForms results eidAufgabe ktyp msignedA atyp mvorlageKonfiguration meinsendung = do
  let (mserver, mtyp, mvorlage, mkonfiguration, meinstellungen, mtesten, _) = results
  aufgabenTypen <- liftM (fmap taskTreeToTextTree . concat) $ mapM (lift . get_task_types . unpack) $ maybeToList mserver
  let ziel = case eidAufgabe of
        Left gruppe -> AufgabeAnlegenR gruppe
        Right aufgabe -> let ANr aufgabeId = A.anr aufgabe
                         in AufgabeR aufgabeId
      firstJust a b = maybe b Just a
      getServerForm ms = getForm ServerForm ziel [] $ serverForm ms
      getTypForm s mt = getForm AufgabeTypForm ziel [] $ typForm aufgabenTypen s mt
      getVorlagenForm s t mv = getForm VorlagenForm ziel [("class", "form-inline")] $ vorlagenForm s t mv
      getKonfigurationForm s t v mk = getForm KonfigurationForm ziel [] $ konfigurationForm ktyp s t v mk
      getAufgabeForm s t v k ma = getForm AufgabeForm ziel [] $ aufgabeForm eidAufgabe s t v k ma
      getHochladenForm s t v k a = getForm HochladenForm ziel [] $ hochladenForm s t v k a
      getTestenForm s t v k a ml = getForm TestenForm ziel [] $ testenForm atyp msignedA s t v k a ml
  sequence $ concat $ fmap maybeToList
    [getServerForm <$> Just mserver,
     getTypForm <$> mserver <*> Just mtyp,
     getVorlagenForm <$> mserver <*> mtyp <*> Just mvorlage,
     getKonfigurationForm <$> mserver <*> mtyp <*> mvorlage <*> Just (firstJust mkonfiguration mvorlageKonfiguration),
     getAufgabeForm <$> mserver <*> mtyp <*> mvorlage <*> mkonfiguration <*> Just meinstellungen,
     getHochladenForm <$> mserver <*> mtyp <*> mvorlage <*> mkonfiguration <*> meinstellungen,
     getTestenForm <$> mserver <*> mtyp <*> mvorlage <*> mkonfiguration <*> meinstellungen <*> Just (firstJust mtesten meinsendung)]

getFormResults :: Either a A.Aufgabe -> Handler FormResults
getFormResults eidAufgabe = do
  (s_, t_, v_, k_, a_) <- getUnsafePostParams -- Achtung: Werte können undefined sein
  ((serverResult, _), _) <- runFormPost $ serverForm Nothing
  ((typResult, _), _) <- runFormPost $ typForm [] s_ Nothing
  ((vorlageResult, _), _) <- runFormPost $ vorlagenForm s_ t_ Nothing
  ((konfigurationResult, _), _) <- runFormPost $ konfigurationForm ("" :: Text) s_ t_ (Just v_) Nothing
  ((einstellungenResult, _), _) <- runFormPost $ aufgabeForm eidAufgabe s_ t_ (Just v_) k_ Nothing
  ((hochladenResult, _), _) <- runFormPost $ hochladenForm s_ t_ (Just v_) k_ a_
  ((testenResult, _), _) <- runFormPost $ testenForm ("" :: Text) Nothing s_ t_ (Just v_) k_ a_ Nothing
  hochladen <- auswerten hochladenResult Tuple6_6 [ServerForm, AufgabeTypForm, VorlagenForm, KonfigurationForm, AufgabeForm]
  eingaben <- do
    a <- auswertenTyped serverResult Tuple6_1 []
    b <- auswertenTyped typResult Tuple6_2 [ServerForm]
    c <- auswertenTyped vorlageResult Tuple6_3 [ServerForm, AufgabeTypForm]
    d <- auswertenTyped konfigurationResult Tuple6_4 [ServerForm, AufgabeTypForm, VorlagenForm]
    e <- auswertenTyped einstellungenResult Tuple6_5 [ServerForm, AufgabeTypForm, VorlagenForm, KonfigurationForm]
    f <- auswertenTyped testenResult Tuple6_6 [ServerForm, AufgabeTypForm, VorlagenForm, KonfigurationForm, AufgabeForm]
    return $ do {_ <- a; _ <- b; _ <- c; _ <- d; _ <- e; f}
  let (mserver', mtyp', mvorlage', mkonfiguration', meinstellungen', mtesten) = fromTuple6 $ either id id eingaben
      (mserver'', mtyp'', mvorlage'', mkonfiguration'', meinstellungen'', mhochladen) = fromTuple6 $ either id id hochladen
      (mserver, mtyp, mvorlage, mkonfiguration, meinstellungen) = case mserver'' of
        Nothing -> (mserver', mtyp', mvorlage', mkonfiguration', meinstellungen')
        Just _ -> (mserver'', mtyp'', mvorlage'', mkonfiguration'', meinstellungen'')
      firstJust a b = maybe b Just a
      maufgabe = either (const Nothing) Just eidAufgabe
  return (firstJust mserver $ pack . toString . A.server <$> maufgabe,
          firstJust mtyp $ pack . toString . A.typ <$> maufgabe,
          firstJust mvorlage $ Just . pack . toString . A.name <$> maufgabe,
          firstJust mkonfiguration $ pack . toString . A.config <$> maufgabe,
          firstJust meinstellungen $ AE.aufgabeToFormDaten <$> maufgabe,
          mtesten,
          mhochladen)

auswertenTyped ::
  (MonadHandler m, RenderMessage (HandlerSite m) FormMessage, a ~ ServerUrl, b ~ AufgabeTyp, c ~ (Maybe VorlageName),
   d ~ AufgabeKonfiguration, e ~ AE.AufgabeFormDaten, f ~ Text) =>
  FormResult t -> (t -> Tuple6 a b c d e f) -> [AufgabeForm]
  -> m (Either (Tuple6 a b c d e f) (Tuple6 a b c d e f))
auswertenTyped = auswerten

auswerten ::
  (MonadHandler m, PathPiece a, PathPiece b, PathPiece c, PathPiece d, PathPiece e,
   RenderMessage (HandlerSite m) FormMessage) =>
  FormResult t -> (t -> Tuple6 a b c d e f) -> [AufgabeForm]
  -> m (Either (Tuple6 a b c d e f) (Tuple6 a b c d e f))
auswerten result t forms =
  case result of
    FormSuccess s -> return $ Left $ t s
    FormFailure _ -> do v <- parameterAbrufen forms
                        return $ Left v
    FormMissing -> return $ Right Tuple6_0

parameterAbrufen :: (MonadHandler m, PathPiece a, PathPiece b, PathPiece c, PathPiece d,
            PathPiece e, RenderMessage (HandlerSite m) FormMessage) =>
           [AufgabeForm] -> m (Tuple6 a b c d e f)
parameterAbrufen forms = do
    let (form1, form2, form3, form4, form5, _) = fromTuple6 $ listToTuple6 forms
        fv = (\ f -> runInputPost $ ireq hiddenField (getId f))
    form1' <- mapM fv form1
    form2' <- mapM fv form2
    form3' <- mapM fv form3
    form4' <- mapM fv form4
    form5' <- mapM fv form5
    return $ toTuple6 (form1', form2', form3', form4', form5', Nothing)

getForm :: AufgabeForm -> Route Autotool -> [(Text, Text)] -> Form a -> Handler AutotoolForm
getForm art ziel attrs form = do
  ((_, widget'), enctype') <- runFormPost form
  return $ AutotoolForm (getTitel art) (getId art) enctype' ziel widget' attrs

tabWidget :: [AutotoolForm] -> [(Text, AutotoolMessage)] -> WidgetT Autotool IO ()
tabWidget forms names = do
  let nameUndTitel ::  AutotoolForm -> (Text, AutotoolMessage)
      nameUndTitel a = (name a, titel a)
  [whamlet|
  $newline never
  $if null forms
    <ul .nav .nav-tabs>
      $forall name' <- names
        <li>
          <a href=##{fst name'}>_{snd name'}
  $else
    $with form <- head forms
      <div ##{name form}>
        ^{tabWidget (tail forms) $ nameUndTitel form : names}
        <div .content>
          <h2>_{titel form}
          <form role="form" action=@{route form} method="post" enctype=#{enctype form} *{attributes form}>
            ^{widget form}
  |]
