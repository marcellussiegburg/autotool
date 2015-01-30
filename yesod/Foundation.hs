module Foundation where

import Prelude
import Yesod
import Yesod.Auth
import Yesod.Auth.Autotool
import qualified Yesod.Auth.Message as AM
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.I18n.English
import Yesod.Form.I18n.German
import Yesod.Form.Jquery
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM, when)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Either (EitherT (runEitherT), left)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Set (member)
import Data.Text (Text, pack)
import Data.Text.Read (decimal, signed)
import Data.Time (UTCTime, formatTime)
import Data.Tuple6
import System.Locale (defaultTimeLocale)
import Model

import Control.Types
import qualified Control.Admin.DB as AdminDB
import qualified Control.Aufgabe.DB as AufgabeDB
import qualified Control.Aufgabe.Typ as Aufgabe
import qualified Control.Direktor.DB as DirektorDB
import qualified Control.Gruppe.DB as GruppeDB
import qualified Control.Gruppe.Typ as Gruppe
import qualified Control.Schule.DB as SchuleDB
import qualified Control.Schule.Typ as Schule
import qualified Control.Semester.DB as SemesterDB
import qualified Control.Semester.Typ as Semester
import qualified Control.Student.DB as StudDB
import qualified Control.Student.Type as Student
import qualified Control.Tutor.DB as TutorDB
import qualified Control.Vorlesung.Typ as Vorlesung
import qualified Control.Vorlesung.DB as VorlesungDB

data Autotool = Autotool
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static
    , httpManager :: Manager
    , appLogger :: Logger
    }

instance HasHttpManager Autotool where
    getHttpManager = httpManager

plural :: Num a => Eq a => a -> Text ->  Text -> Text
plural zahl einzahl mehrzahl =
  case zahl of
    1 -> einzahl
    _ -> mehrzahl

mkMessage "Autotool" "messages" "de"

mkYesodData "Autotool" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT Autotool IO) (FormResult x, Widget)

instance Yesod Autotool where
    approot = ApprootMaster $ appRoot . settings

    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        maid <- maybeAuthId
        mroute <- getCurrentRoute
        lang <- fmap (\langs -> if langs == [] then "de" else head langs) languages
        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_min_css
                -- , css_bootstrap_theme_min_css
                , css_autotool_css
                ])
            $(combineScripts 'StaticR
                [ js_jquery_min_js
                , js_bootstrap_js
                ])
            navMenu <- lift $ navigationMenu mroute maid
            let navigation = $(widgetFile "navigation")
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    isAuthorized route _ = do
      mid <- maybeAuthId
      mautorisiert <- lift $ istAutorisiert mid route
      case mautorisiert of
        Nothing -> return AuthenticationRequired
        Just hatZugriff ->
          if hatZugriff
              then return Authorized
              else unauthorizedI MsgNichtAutorisiert

    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    jsLoader _ = BottomOfBody

    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger
    authRoute _ = Just $ AuthR LoginR

instance YesodJquery Autotool where
    urlJqueryJs _ = Left $ StaticR js_jquery_min_js
    urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_min_js
    urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_min_css
    urlJqueryUiDateTimePicker _ = Left $ StaticR js_jquery_ui_datetimepicker_js

instance RenderMessage Autotool FormMessage where
    renderMessage _ ("de":_) = germanFormMessage
    renderMessage _ ("en":_) = englishFormMessage
    renderMessage master (_:langs) = renderMessage master langs
    renderMessage _ [] = defaultFormMessage

instance RenderMessage Autotool YesodAuthAutotoolMessage where
    renderMessage _ ("de":_) = germanAuthAutotoolMessage
    renderMessage _ ("en":_) = englishAuthAutotoolMessage
    renderMessage master (_:langs) = renderMessage master langs
    renderMessage _ [] = germanAuthAutotoolMessage

instance YesodAuth Autotool where
    renderAuthMessage _ ("de":_) = AM.germanMessage
    renderAuthMessage _ ("en":_) = AM.englishMessage
    renderAuthMessage master (_:langs) = renderAuthMessage master langs
    renderAuthMessage _ [] = AM.defaultMessage
    type AuthId Autotool = Int
    loginDest _ = SchulenR
    logoutDest _ = SchulenR
    getAuthId creds = liftIO $ do
      case signed decimal $ credsIdent creds of
        Right (s, "") -> do
          x <- StudDB.get_snr $ SNr s
          case x of
            [_] -> return $ Just $ s
            _ -> return $ Nothing
        _ -> return Nothing
    maybeAuthId = do
      ms <- lookupSession credsKey
      case ms of
        Nothing -> return Nothing
        Just session ->
            case fromPathPiece session of
              Nothing -> return Nothing
              Just s -> do
                mstud <- liftIO $ StudDB.get_snr $ SNr s
                case mstud of
                  [_] -> return $ Just s
                  _ -> return Nothing
    authPlugins _ = [authAutotool $ Nothing]
    authHttpManager = httpManager

instance YesodAuthAutotool Autotool where
   type AuthSchool = UNr
   type AuthStudent = MNr
   getStudentByMNr u m = liftIO $ StudDB.get_unr_mnr (u, m)
   getStudentByAuthStudent u m = liftIO $ StudDB.get_unr_mnr (u, m)
   getSchools = liftIO SchuleDB.get
   getSchool = maybe (return Nothing) $ \u' -> liftIO $ SchuleDB.get_unr u' >>= return . listToMaybe
   toSchool u' = liftIO $ SchuleDB.get_unr (UNr u') >>= return . listToMaybe
   studentToAuthStudent = return . Student.mnr
   schoolToAuthSchool = return . Schule.unr

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- | Gibt zurück, ob ein Nutzer berechtigt ist, auf eine Route @route@ zuzugreifen. Dabei wird 'Just True' geliefert, wenn der Nutzer berechtigt ist. 'Nothing' wird geliefert, wenn der Nutzer nicht angemeldet ist (@mid@ = 'Nothing') und Authentifizierung notwendig ist, um auf die Ressource zuzugreifen. 'Just False' wird geliefert, wenn der Nutzer authentifiziert ist, allerdings nicht die notwendige Autorisierung besitzt.
istAutorisiert :: Maybe (AuthId Autotool) -> Route Autotool -> IO (Maybe Bool)
istAutorisiert mid route =
  let braucht a = if a `member` routeAttrs route
                  then [a] else []
      attrs = concat [braucht "admin", braucht "direktor", braucht "jederTutor", braucht "tutor", braucht "student", braucht "einschreibung"]
  in case attrs of
       [] -> return $ Just True
       _ -> runMaybeT $ do
         aId <- MaybeT $ return mid
         lift $ foldlM (\c a ->
           if c == True
           then return True
           else autorisierungRolle a aId route
           ) False attrs

-- | Liefert 'True', wenn der Nutzer mit der Kennung @authId@ und der Rolle @rolle@ autorisiert ist, auf die Route @route@ zuzugreifen.
autorisierungRolle:: Text -> Int -> Route Autotool -> IO Bool
autorisierungRolle rolle authId route
  | rolle == "student" = do
      auth <- runEitherT $ do
        (mschule, _, _, _, _, _) <- lift $ routeInformation route
        mstud <- lift $ StudDB.get_snr $ SNr authId
        stud <- fromMaybe (left False) $ fmap return $ listToMaybe mstud
        schule <- fromMaybe (left False) $ fmap return mschule
        return $ Student.unr stud == UNr schule
      return $ either id id auth
  | rolle == "einschreibung" = do
      auth <- runEitherT $ do
        (mschule, _, mvorlesung, _, _, _) <- lift $ routeInformation route
        mstud <- lift $ StudDB.get_snr $ SNr authId
        stud <- fromMaybe (left False) $ fmap return $ listToMaybe mstud
        schule <- fromMaybe (left False) $ fmap return mschule
        when (Student.unr stud /= UNr schule) $ left False
        vorlesung <- fromMaybe (left False) $ fmap return mvorlesung
        vorlesungen <- lift $ VorlesungDB.get_attended $ SNr authId
        return $ elem (VNr vorlesung) $ map Vorlesung.vnr vorlesungen
      return $ either id id auth
  | rolle == "admin" = do
      auth <- runMaybeT $ do
        mstud <- lift $ StudDB.get_snr (SNr authId)
        stud <- MaybeT $ return $ listToMaybe mstud
        lift $ AdminDB.is_minister stud
      return $ maybe False id auth
  | rolle == "direktor" = do
      auth <- runMaybeT $ do
        mstud <- lift $ StudDB.get_snr (SNr authId)
        stud <- MaybeT $ return $ listToMaybe mstud
        schulen <- lift $ DirektorDB.get_directed stud
        (mschule,_,_,_,_,_) <- lift $ routeInformation route
        schule <- MaybeT $ return mschule
        return $ elem (UNr schule) $ map Schule.unr schulen
      return $ maybe False id auth
  | rolle == "tutor" = do
      auth <- runMaybeT $ do
        mstud <- lift $ StudDB.get_snr (SNr authId)
        stud <- MaybeT $ return $ listToMaybe mstud
        vorlesungen <- lift $ TutorDB.get_tutored stud
        (_,_,mvorlesung,_,_,_) <- lift $ routeInformation route
        vorlesung <- MaybeT $ return mvorlesung
        return $ elem (VNr vorlesung) $ map Vorlesung.vnr vorlesungen
      return $ maybe False id auth
  | rolle == "jederTutor" = do
      auth <- runMaybeT $ do
        mstud <- lift $ StudDB.get_snr (SNr authId)
        stud <- MaybeT $ return $ listToMaybe mstud
        vorlesungen <- lift $ TutorDB.get_tutored stud
        (mschule,_,_,_,_,_) <- lift $ routeInformation route
        schule <- MaybeT $ return mschule
        return $ elem (UNr schule) $ map Vorlesung.unr vorlesungen
      return $ maybe False id auth
  | otherwise =
      return $ False

data NavigationMenu = NavigationMenu AutotoolMessage [NavigationEntry]

data NavigationEntry = Trennstrich | Titel Text | Link (Route Autotool) (Maybe AutotoolMessage)

-- | Liefert das Navigationsmenü, die zu verwendenden Einträge sind in dieser Methode definiert.
navigationMenu :: Maybe (Route Autotool) -> Maybe Int -> IO [NavigationMenu]
navigationMenu mroute authId = do
  let schule s = [SchuleR s, DirektorenR s, DirektorErnennenR s, WaisenkinderR s, SemestersR s, SemesterAnlegenR s]
      schulen = [SchulenR, SchuleAnlegenR] -- ^ TODO ++ [Persönliche Daten, Highscore]
      semester s = [SemesterR s, VorlesungenR s, VorlesungAnlegenR s]
      vorlesung v = [VorlesungR v, TutorenR v, TutorErnennenR v, StudentenR v, ResultateR v, ResultatePflichtR v, GruppenR v, GruppeAnlegenR v, AufgabenR v, AufgabenAktuellR v, AufgabeAnlegenR v]
      gruppe g = [GruppeR g]
      aufgabe a = [AufgabeBearbeitenR a, AufgabeR a, StatistikR a]
      servers = [ServersR]
      server s t v k = concat $ map maybeToList
        [ServerR <$> s
        ,AufgabeVorlagenR <$> s <*> t
        ,AufgabeVorlageR <$> s <*> t <*> v
        ,AufgabeKonfigurationR <$> s <*> t <*> k
        ,AufgabeTestenR <$> s <*> t <*> k]
      (as, at, av, ak) = serverRouteInformation mroute
  (schu, sem, vorl, grup, aufg) <- routeBrotkrumen mroute authId
  let server' = server as at av ak
      filterRoute r = do hatZugriff <- istAutorisiert authId r
                         return $ maybe False id hatZugriff
      filterBerechtigte liste param =
        filterM filterRoute $ concat . maybeToList $ liste <$> param
      zuLink r = Link r $ routeTitel r
      trennstrich [] b = b
      trennstrich a [] = a
      trennstrich a b = a ++ [Trennstrich] ++ b
      addTitel _ [] = []
      addTitel Nothing _ = []
      addTitel t l = map Titel (maybeToList t) ++ l
  schule' <- filterBerechtigte schule schu
  schulen' <- filterM filterRoute schulen
  semester' <- filterBerechtigte semester sem
  vorlesung' <- filterBerechtigte vorlesung vorl
  gruppe' <- filterBerechtigte gruppe grup
  aufgabe' <- filterBerechtigte aufgabe aufg
  schuName <- schuleName schu
  semName <- semesterName sem
  vorlName <- vorlesungName vorl
  grupName <- gruppeName grup
  aufgName <- aufgabeName aufg
  return [NavigationMenu MsgSchule $ trennstrich (addTitel schuName $ map zuLink schule') $ map zuLink schulen'
         ,NavigationMenu MsgSemester $ addTitel semName $ map zuLink semester'
         ,NavigationMenu MsgVorlesung $ addTitel vorlName $ map zuLink vorlesung'
         ,NavigationMenu MsgGruppe $ addTitel grupName $ map zuLink gruppe'
         ,NavigationMenu MsgAufgabe $ trennstrich (addTitel aufgName $ map zuLink aufgabe') $ map zuLink $ servers ++ server']

-- | Liefert den Schulnamen zur Id der Schule
schuleName :: Maybe SchuleId -> IO (Maybe Text)
schuleName = getName UNr SchuleDB.get_unr Schule.name

-- | Liefert den Semesternamen zur Id des Semesters
semesterName :: Maybe SemesterId -> IO (Maybe Text)
semesterName = getName ENr SemesterDB.get_this Semester.name

-- | Liefert den Vorlesungsnamen zur Id der Vorlesung
vorlesungName :: Maybe VorlesungId -> IO (Maybe Text)
vorlesungName = getName VNr VorlesungDB.get_this Vorlesung.name

-- | Liefert den Gruppennamen zur Id der Gruppe
gruppeName :: Maybe GruppeId -> IO (Maybe Text)
gruppeName = getName GNr GruppeDB.get_gnr Gruppe.name

-- | Liefert den Aufgabenamen zur Id der Aufgabe
aufgabeName :: Maybe AufgabeId -> IO (Maybe Text)
aufgabeName = getName ANr AufgabeDB.get_this Aufgabe.name

-- | Liefert den Namen zu Werten von DB-Einträgen. Benötigt den @konstruktor@, der Schlüsselwerte, die @dbFunktion@ zum Abrufen der DB-Einträge, die Funktion @nameFunktion@, die die Namen der DB-Einträge liefert und die Id, die dem Konstruktor übergeben wird @mvId@ (kann Nothing sein).
getName :: Monad m => (a -> b) -> (b -> m [c]) -> (c -> Name) -> Maybe a -> m (Maybe Text)
getName konstruktor dbFunktion nameFunktion mvId = runMaybeT $ do
  vId <- MaybeT . return $ mvId
  vs <- lift $ dbFunktion $ konstruktor vId
  v <- MaybeT . return $ listToMaybe vs
  let Name sname = nameFunktion v
  return $ pack sname

-- | Liefert die für die Navigation notwendigen Ids der Datenbankeinträge, abhängig von der angegebenen Route und der Relevanz für den Nutzer. Route Nothing ist für den Fall einer fehlerhaften URL vorgesehen.
-- 
-- Momentan unterscheidet sich die Funktion nur in der Anzahl der Elemente des Tupels, das zurückgeliefert wird, von routeInformation.
routeBrotkrumen :: Maybe (Route Autotool) -> Maybe Int -> IO (Maybe SchuleId, Maybe SemesterId, Maybe VorlesungId, Maybe GruppeId, Maybe AufgabeId)
routeBrotkrumen Nothing _ = return (Nothing, Nothing, Nothing, Nothing, Nothing)
routeBrotkrumen (Just route) _ = do
  (schule, semester, vorlesung, gruppe, aufgabe, _) <- routeInformation route
  return (schule, semester, vorlesung, gruppe, aufgabe)

-- | Liefert die für die Navigation notwendigen Ids der Datenbankeinträge abhängig von der angegebenen Route und unabhängig von der Relevanz für den Nutzer. Dient als Hilfsfunktion für @routeBrotkrumen@.
routeInformation :: Route Autotool -> IO (Maybe SchuleId, Maybe SemesterId, Maybe VorlesungId, Maybe GruppeId, Maybe AufgabeId, Maybe ())
routeInformation route = case routeParameter route of
   Nothing -> return $ fromTuple6 Tuple6_0
   Just parameter -> case parameter of
     SchuleRoute schule -> return $ fromTuple6 $ Tuple6_1 schule
     SemesterRoute semester -> do
       schule <- getSchule $ Just semester
       return $ fromTuple6 $ toTuple6 (schule, Just semester, Nothing, Nothing, Nothing, Nothing)
     VorlesungRoute vorlesung -> do
       semester <- getSemester $ Just vorlesung
       schule <- getSchule semester
       return $ fromTuple6 $ toTuple6 (schule, semester, Just vorlesung, Nothing, Nothing, Nothing)
     GruppeRoute gruppe -> do
       vorlesung <- getVorlesung $ Just gruppe
       semester <- getSemester vorlesung
       schule <- getSchule semester
       return $ fromTuple6 $ toTuple6 (schule, semester, vorlesung, Just gruppe, Nothing, Nothing)
     AufgabeRoute aufgabe -> do
       vorlesung <- getVorlesungAufgabe $ Just aufgabe
       semester <- getSemester vorlesung
       schule <- getSchule semester
       case fromTuple6 $ toTuple6 (schule, semester, vorlesung, Nothing, Nothing, Nothing) of
         (Just _, Just _, Just _, _,_,_) -> return (schule, semester, vorlesung, Nothing, Just aufgabe, Nothing)
         _ -> return $ fromTuple6 Tuple6_0
     ServerRoute _ -> return $ fromTuple6 Tuple6_0

-- | Liefert für die Interaktion mit dem Semantik-Server die aktuell abrufbaren Parameter aus der angegebenen Route.
serverRouteInformation :: Maybe (Route Autotool) -> (Maybe ServerUrl, Maybe AufgabeTyp, Maybe VorlageName, Maybe AufgabeKonfiguration)
serverRouteInformation mroute = case mroute of
  Nothing -> (Nothing, Nothing, Nothing, Nothing)
  Just route -> case routeParameter route of
    Just (ServerRoute param) ->  case param of
      ServerUrlRoute s -> (Just s, Nothing, Nothing, Nothing)
      VorlagenRoute s t -> (Just s, Just t, Nothing, Nothing)
      VorlageRoute s t v -> (Just s, Just t, Just v, Nothing)
      KonfigurationRoute s t k -> (Just s, Just t, Nothing, Just k)
    _ -> (Nothing, Nothing, Nothing, Nothing)

-- | Liefert ggf. die Schule zu einem Semester
getSchule :: Maybe SemesterId -> IO (Maybe SchuleId)
getSchule msemester = runMaybeT $ do
  semester <- MaybeT $ return msemester
  semesters <- lift $ SemesterDB.get_this $ ENr semester
  UNr schuleId <- MaybeT $ return $ listToMaybe $ map Semester.unr semesters
  return schuleId

-- | Liefert ggf. das Semester zu einer Vorlesung
getSemester :: Maybe VorlesungId -> IO (Maybe SemesterId)
getSemester mvorlesung = runMaybeT $ do
  vorlesung <- MaybeT $ return mvorlesung
  vorlesungen <- lift $ VorlesungDB.get_this $ VNr vorlesung
  ENr semesterId <- MaybeT $ return $ listToMaybe $ map Vorlesung.enr vorlesungen
  return semesterId

-- | Liefert ggf. die Vorlesung zu einer Gruppe
getVorlesung :: Maybe GruppeId -> IO (Maybe VorlesungId)
getVorlesung mgruppe  = runMaybeT $ do
  gruppe <- MaybeT $ return mgruppe
  gruppen <- lift $ GruppeDB.get_gnr $ GNr gruppe
  VNr vorlesungId <- MaybeT $ return $ listToMaybe $ map Gruppe.vnr gruppen
  return vorlesungId

-- | Liefert ggf. die Vorlesung zu einer Aufgabe
getVorlesungAufgabe :: Maybe AufgabeId -> IO (Maybe VorlesungId)
getVorlesungAufgabe maufgabe  = runMaybeT $ do
  aufgabe <- MaybeT $ return maufgabe
  aufgaben <- lift $ AufgabeDB.get_this $ ANr aufgabe
  VNr vorlesungId <- MaybeT $ return $ listToMaybe $ map Aufgabe.vnr aufgaben
  return vorlesungId

data RouteParameter =
    SchuleRoute SchuleId
  | SemesterRoute SemesterId
  | VorlesungRoute VorlesungId
  | GruppeRoute GruppeId
  | AufgabeRoute AufgabeId
  | ServerRoute ServerParameter

data ServerParameter =
   ServerUrlRoute ServerUrl
 | VorlagenRoute ServerUrl AufgabeTyp
 | VorlageRoute ServerUrl AufgabeTyp VorlageName
 | KonfigurationRoute ServerUrl AufgabeTyp AufgabeKonfiguration

-- | Liefert den Parameter, der der Route übergeben wurde
-- 
-- Diese Methode nutzt absichtlich keinen Wildcard-Eintrag, um Rückmeldung vom Compiler bei neu definierten Routen zu erhalten
routeParameter :: Route Autotool -> Maybe RouteParameter
routeParameter route = case route of
  StaticR _                   -> Nothing
  FaviconR                    -> Nothing
  RobotsR                     -> Nothing
  AuthR _                     -> Nothing
  HomeR                       -> Nothing
  SchuleAnlegenR              -> Nothing
  SchulenR                    -> Nothing
  SchuleR s                   -> Just $ SchuleRoute s
  DirektorenR s               -> Just $ SchuleRoute s
  DirektorErnennenR s         -> Just $ SchuleRoute s
  WaisenkinderR s             -> Just $ SchuleRoute s
  SemestersR s                -> Just $ SchuleRoute s
  SemesterAnlegenR s          -> Just $ SchuleRoute s
  SemesterR s                 -> Just $ SemesterRoute s
  VorlesungenR s              -> Just $ SemesterRoute s
  VorlesungAnlegenR s         -> Just $ SemesterRoute s
  VorlesungR v                -> Just $ VorlesungRoute v
  GruppeAnlegenR v            -> Just $ VorlesungRoute v
  GruppenR v                  -> Just $ VorlesungRoute v
  StudentenR v                -> Just $ VorlesungRoute v
  TutorErnennenR v            -> Just $ VorlesungRoute v
  TutorenR v                  -> Just $ VorlesungRoute v
  ResultateR v                -> Just $ VorlesungRoute v
  ResultatePflichtR v         -> Just $ VorlesungRoute v
  AufgabeAnlegenR v           -> Just $ VorlesungRoute v
  AufgabenR v                 -> Just $ VorlesungRoute v
  AufgabenAktuellR v          -> Just $ VorlesungRoute v
  GruppeR g                   -> Just $ GruppeRoute g
  AufgabeBearbeitenR a        -> Just $ AufgabeRoute a
  AufgabeR a                  -> Just $ AufgabeRoute a
  StatistikR a                -> Just $ AufgabeRoute a
  ServersR                    -> Nothing
  ServerR s                   -> Just $ ServerRoute $ ServerUrlRoute s
  AufgabeVorlagenR s t        -> Just $ ServerRoute $ VorlagenRoute s t
  AufgabeVorlageR s t v       -> Just $ ServerRoute $ VorlageRoute s t v
  AufgabeKonfigurationR s t k -> Just $ ServerRoute $ KonfigurationRoute s t k
  AufgabeTestenR s t k        -> Just $ ServerRoute $ KonfigurationRoute s t k

-- | Liefert den Titel, der für einen Link zu dieser Route verwendet werden soll
-- 
-- Diese Methode nutzt absichtlich keinen Wildcard-Eintrag, um Rückmeldung vom Compiler bei neu definierten Routen zu erhalten
routeTitel :: Route Autotool -> Maybe AutotoolMessage
routeTitel route = case route of
  StaticR _                   -> Nothing
  FaviconR                    -> Nothing
  RobotsR                     -> Nothing
  AuthR _                     -> Nothing
  HomeR                       -> Nothing
  SchuleAnlegenR              -> Just MsgSchuleAnlegen
  SchulenR                    -> Just MsgSchulen
  SchuleR _                   -> Just MsgBearbeiten
  DirektorenR _               -> Just MsgDirektoren
  DirektorErnennenR _         -> Just MsgDirektorErnennen
  WaisenkinderR _             -> Just MsgWaisenkinder
  SemestersR _                -> Just MsgSemesters
  SemesterAnlegenR _          -> Just MsgSemesterAnlegen
  SemesterR _                 -> Just MsgBearbeiten
  VorlesungenR _              -> Just MsgVorlesungen
  VorlesungAnlegenR _         -> Just MsgVorlesungAnlegen
  VorlesungR _                -> Just MsgBearbeiten
  GruppeAnlegenR _            -> Just MsgGruppeAnlegen
  GruppenR _                  -> Just MsgGruppen
  StudentenR _                -> Just MsgStudenten
  TutorErnennenR _            -> Just MsgTutor
  TutorenR _                  -> Just MsgTutoren
  ResultateR _                -> Just MsgAlleResultate
  ResultatePflichtR _         -> Just MsgResultatePflicht
  AufgabeAnlegenR _           -> Just MsgAufgabeAnlegen
  AufgabenR _                 -> Just MsgAufgaben
  AufgabenAktuellR _          -> Just MsgAufgabenAktuell
  GruppeR _                   -> Just MsgBearbeiten
  AufgabeBearbeitenR _        -> Just MsgBearbeiten
  AufgabeR _                  -> Just MsgLösen
  StatistikR _                -> Just MsgStatistikAnzeigen
  ServersR                    -> Just MsgServers
  ServerR _                   -> Just MsgAufgabeTyp
  AufgabeVorlagenR _ _        -> Just MsgAufgabeVorlagen
  AufgabeVorlageR _ _ _       -> Just MsgAufgabeKonfigurieren
  AufgabeKonfigurationR _ _ _ -> Just MsgAufgabeKonfigurieren
  AufgabeTestenR _ _ _        -> Just MsgAufgabeTesten
