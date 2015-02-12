module Handler.AufgabeAnlegen where

import Import

import qualified Control.Exception as Exception
import Data.List (head, tail)
import Data.Tuple6

import Handler.Aufgabe.Forms
import Handler.AufgabeEinstellungen (AufgabeFormDaten, aufgabeToFormDaten)
import Handler.AufgabeVorlage (getVorlageKonfiguration)
import Handler.AufgabeKonfiguration (checkKonfiguration, getBeispielKonfiguration, getKonfigurationFehler)
import Handler.AufgabeTesten (getAufgabeInstanz, getBewertung)

import qualified Control.Aufgabe.DB as AufgabeDB
import qualified Control.Aufgabe.Typ as A (Aufgabe (..))
import Control.Types
import Service.Interface (get_task_types)

data AutotoolForm = AutotoolForm {
  titel :: AutotoolMessage,
  name :: Text,
  enctype :: Enctype,
  route :: Route Autotool,
  widget :: Widget,
  attributes :: [(Text, Text)]
}

getAufgabeAnlegenR :: VorlesungId -> Handler Html
getAufgabeAnlegenR = postAufgabeAnlegenR

postAufgabeAnlegenR :: VorlesungId -> Handler Html
postAufgabeAnlegenR vorlesung = do
  aufgabeTemplate (Left vorlesung) Nothing

aufgabeTemplate :: Either VorlesungId AufgabeId -> Maybe A.Aufgabe -> Handler Html
aufgabeTemplate eid maufgabe = do
  let mhinweis = Nothing :: Maybe Text
  results <- getFormResults eid maufgabe
  let (mserver, mtyp, mvorlage, mkonfiguration, meinstellungen, _, mhochladen) = results
  (beispielKonfiguration, ktyp) <- maybe (return ("","")) id $
    getBeispielKonfiguration <$> mserver <*> mtyp
  mvorlageKonfiguration <- sequence $ getVorlageKonfiguration <$> Just beispielKonfiguration <*> mtyp <*> (maybe "" id <$> mvorlage)
  mhinweisFehler <- liftM join $ sequence $ getKonfigurationFehler <$> mserver <*> mtyp <*> maybe mvorlageKonfiguration Just mkonfiguration
  signedK <- liftM (>>= rightToMaybe) $
    sequence $ checkKonfiguration <$> mserver <*> mtyp <*> mkonfiguration
  (msignedA, meinsendung, atyp, maufgabenstellung) <-
    liftM (maybe (Nothing, Nothing, "" :: Html, Nothing)
        (\ (a, b, c, d) -> (Just a, Just b, c, Just d))) $
    sequence $ getAufgabeInstanz <$> mserver <*> signedK <*> Just "11111" -- ^ TODO auswählbar machen
  mbewertung <- liftM join $ sequence $ getBewertung <$> mserver <*> msignedA <*> Just mhochladen
  forms <- getForms results eid ktyp msignedA atyp mvorlageKonfiguration meinsendung
  defaultLayout $ do
    addStylesheet $ StaticR css_tree_css
    $(widgetFile "aufgabeAnlegen")

getForms results eid ktyp msignedA atyp mvorlageKonfiguration meinsendung = do
  let (mserver, mtyp, mvorlage, mkonfiguration, meinstellungen, mtesten, _) = results
  aufgabenTypen <- liftM (fmap taskTreeToTextTree . concat) $ mapM (lift . get_task_types . unpack) $ maybeToList mserver
  vorlagen <- liftM ((fmap $ pack . toString . A.name) . concat) $ mapM
    (\ typ ->
      lift $ AufgabeDB.get_typed (fromCGI $ unpack typ)
        `Exception.catch` \ (Exception.SomeException _) -> return []
    ) $ maybeToList mtyp
  let ziel = case eid of
        Left gruppe -> AufgabeAnlegenR gruppe
        Right aufgabe -> AufgabeBearbeitenR aufgabe
      firstJust a b = maybe b Just a
      getServerForm ms = getForm ServerForm ziel [] $ serverForm ms
      getTypForm s mt = getForm AufgabeTypForm ziel [] $ typForm aufgabenTypen s mt
      getVorlagenForm s t mv = getForm VorlagenForm ziel [("class", "form-inline")] $ vorlagenForm vorlagen s t mv
      getKonfigurationForm s t v mk = getForm KonfigurationForm ziel [] $ konfigurationForm ktyp s t v mk
      getAufgabeForm s t v k ma = getForm AufgabeForm ziel [] $ aufgabeForm eid s t v k ma
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

getFormResults :: Either a b -> Maybe A.Aufgabe -> Handler (Maybe ServerUrl, Maybe AufgabeTyp, Maybe (Maybe VorlageName), Maybe AufgabeKonfiguration, Maybe AufgabeFormDaten, Maybe Text, Maybe FileInfo)
getFormResults eid maufgabe = do
  (s_, t_, v_, k_, a_) <- getUnsafePostParams -- ^ Achtung: Werte können undefined sein
  ((serverResult, _), _) <- runFormPost $ serverForm Nothing
  ((typResult, _), _) <- runFormPost $ typForm [] s_ Nothing
  ((vorlageResult, _), _) <- runFormPost $ vorlagenForm [] s_ t_ Nothing
  ((konfigurationResult, _), _) <- runFormPost $ konfigurationForm ("" :: Text) s_ t_ (Just v_) Nothing
  ((einstellungenResult, _), _) <- runFormPost $ aufgabeForm eid s_ t_ (Just v_) k_ Nothing
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
  return $ case maufgabe of
    Just aufgabe ->
      (Just . pack . toString . A.server $ aufgabe,
       Just . pack . toString . A.typ $ aufgabe,
       Just . Just . pack . toString . A.name $ aufgabe,
       Just . pack . toString . A.config $ aufgabe,
       Just $ aufgabeToFormDaten aufgabe,
       mtesten,
       mhochladen)
    Nothing -> case mserver'' of
      Nothing -> (mserver', mtyp', mvorlage', mkonfiguration', meinstellungen', mtesten, mhochladen)
      Just _ -> (mserver'', mtyp'', mvorlage'', mkonfiguration'', meinstellungen'', mtesten, mhochladen)

auswertenTyped ::
  (MonadHandler m, RenderMessage (HandlerSite m) FormMessage, a ~ ServerUrl, b ~ AufgabeTyp, c ~ (Maybe VorlageName),
   d ~ AufgabeKonfiguration, e ~ AufgabeFormDaten, f ~ Text) =>
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
