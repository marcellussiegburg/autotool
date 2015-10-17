module Foundation where

import Prelude hiding (sequence)
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
import Control.Monad (filterM, join, liftM, when)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Either (EitherT (runEitherT), left)
import Data.Char (toUpper)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Set (member)
import Data.Text (Text, pack, unpack)
import Data.Text.Read (decimal, signed)
import Data.Time (UTCTime, formatTime)
import Data.Traversable (sequence)
import Data.Tuple6
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
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
import qualified Control.Student.DB as StudDB
import qualified Control.Student.Type as Student
import qualified Control.Tutor.DB as TutorDB
import qualified Control.Vorlesung.Typ as Vorlesung
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Autolib.Multilingual as Sprache

data Autotool = Autotool
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
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
        mmtitel <- sequence $ fmap routeTitel mroute
        let mtitel = join mmtitel
        lang <- fmap (\langs -> if langs == [] then "de" else head langs) languages
        pc <- widgetToPageContent $ do
            maybe (return ()) setTitleI mtitel
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
            navMenu <- handlerToWidget $ navigationMenu mroute maid
            let navigation = $(widgetFile "navigation")
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    isAuthorized route _ = do
      mid <- maybeAuthId
      mautorisiert <- istAutorisiert mid route
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

formToWidget :: Route Autotool -> Maybe Text -> Enctype -> Widget -> Widget
formToWidget ziel manker enctype widget =
  case manker of
    Nothing ->
      [whamlet|<form role="form" action=@{ziel} method="post" enctype=#{enctype}>^{widget}|]
    Just anker ->
      [whamlet|<form role="form" action=@{ziel}##{anker} method="post" enctype=#{enctype}>^{widget}|]

getBevorzugteSprache :: MonadHandler m => m Sprache.Language
getBevorzugteSprache = do
  langs <- languages
  return $ getBevorzugteSprache' $ map unpack langs

-- | Sollte angepasst werden, wenn der Datentyp Language verändert wird
-- TODO: Datentyp Language sollte auf die Form "DE_de" geändert werden (erst Sprache, dann Region). (in autolib-todoc)
getBevorzugteSprache' :: [String] -> Sprache.Language
getBevorzugteSprache' [] = Sprache.DE
getBevorzugteSprache' (l:ls) =
  case take 2 (map toUpper l) of
    "UK" -> Sprache.UK
    l' -> case reads l' of
            (l'',_):_ -> l''
            _ -> getBevorzugteSprache' ls

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

instance YesodPersist Autotool where
    type YesodPersistBackend Autotool = SqlBackend
    runDB = defaultRunDB persistConfig connPool

instance YesodPersistRunner Autotool where
    getDBRunner = defaultGetDBRunner connPool

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
istAutorisiert :: Maybe (AuthId Autotool) -> Route Autotool -> Handler (Maybe Bool)
istAutorisiert mid route =
  let braucht a = if a `member` routeAttrs route
                  then [a] else []
      attrs = concat $ map braucht ["admin", "direktor", "jederTutor", "tutor", "student", "einschreibung", "studentEigene"]
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
autorisierungRolle:: Text -> Int -> Route Autotool -> Handler Bool
autorisierungRolle rolle authId route
  | rolle == "student" = do
      auth <- runEitherT $ do
        (mschule, _, _, _, _, _) <- lift $ routeInformation route
        mstud <- lift $ liftIO $ StudDB.get_snr $ SNr authId
        stud <- fromMaybe (left False) $ fmap return $ listToMaybe mstud
        schule <- fromMaybe (left False) $ fmap return mschule
        return $ Student.unr stud == UNr (keyToInt schule)
      return $ either id id auth
  | rolle == "studentEigene" = do
      auth <- runEitherT $ do
        (_, _, _, _, _, mstudentId) <- lift $ routeInformation route
        studentId <- fromMaybe (left False) $ fmap return mstudentId
        return $ studentId == authId
      return $ either id id auth
  | rolle == "einschreibung" = do
      auth <- runEitherT $ do
        (mschule, _, mvorlesung, _, _, _) <- lift $ routeInformation route
        mstud <- lift $ liftIO $ StudDB.get_snr $ SNr authId
        stud <- fromMaybe (left False) $ fmap return $ listToMaybe mstud
        schule <- fromMaybe (left False) $ fmap (return . keyToInt) mschule
        when (Student.unr stud /= UNr schule) $ left False
        vorlesung <- fromMaybe (left False) $ fmap return mvorlesung
        vorlesungen <- lift $ liftIO $ VorlesungDB.get_attended $ SNr authId
        return $ elem (VNr $ keyToInt vorlesung) $ map Vorlesung.vnr vorlesungen
      return $ either id id auth
  | rolle == "admin" = do
      auth <- liftIO $ runMaybeT $ do
        mstud <- lift $ StudDB.get_snr (SNr authId)
        stud <- MaybeT $ return $ listToMaybe mstud
        lift $ AdminDB.is_minister stud
      return $ maybe False id auth
  | rolle == "direktor" = do
      auth <- runMaybeT $ do
        mstud <- lift $ liftIO $ StudDB.get_snr (SNr authId)
        stud <- MaybeT $ return $ listToMaybe mstud
        schulen <- lift $ liftIO $ DirektorDB.get_directed stud
        (mschule,_,_,_,_,_) <- lift $ routeInformation route
        schule <- MaybeT $ return mschule
        return $ elem (UNr $ keyToInt schule) $ map Schule.unr schulen
      return $ maybe False id auth
  | rolle == "tutor" = do
      auth <- runMaybeT $ do
        mstud <- lift $ liftIO $ StudDB.get_snr (SNr authId)
        stud <- MaybeT $ return $ listToMaybe mstud
        vorlesungen <- lift $ liftIO $ TutorDB.get_tutored stud
        (_,_,mvorlesung,_,_,_) <- lift $ routeInformation route
        vorlesung <- MaybeT $ return mvorlesung
        return $ elem (VNr $ keyToInt vorlesung) $ map Vorlesung.vnr vorlesungen
      return $ maybe False id auth
  | rolle == "jederTutor" = do
      auth <- runMaybeT $ do
        mstud <- lift $ liftIO $ StudDB.get_snr (SNr authId)
        stud <- MaybeT $ return $ listToMaybe mstud
        vorlesungen <- lift $ liftIO $ TutorDB.get_tutored stud
        (mschule,_,_,_,_,_) <- lift $ routeInformation route
        schule <- MaybeT $ return mschule
        return $ elem (UNr $ keyToInt schule) $ map Vorlesung.unr vorlesungen
      return $ maybe False id auth
  | otherwise =
      return $ False

data NavigationMenu = NavigationMenu AutotoolMessage [NavigationEntry]

data NavigationEntry = Trennstrich | Titel Text | Link (Route Autotool) (Maybe AutotoolMessage)

-- | Liefert das Navigationsmenü, die zu verwendenden Einträge sind in dieser Methode definiert.
navigationMenu :: Maybe (Route Autotool) -> Maybe Int -> Handler [NavigationMenu]
navigationMenu mroute authId = do
  let schule s = [SchuleR s, DirektorenR s, DirektorErnennenR s, WaisenkinderR s, SemestersR s, SemesterAnlegenR s]
      schulen = [SchulenR, SchuleAnlegenR] -- TODO ++ [Persönliche Daten, Highscore]
      semester s = [SemesterR s, VorlesungenR s, VorlesungAnlegenR s]
      vorlesung v = [VorlesungR v, TutorenR v, TutorErnennenR v, StudentenR v, ResultateR v, ResultatePflichtR v, GruppenR v, GruppeAnlegenR v, AufgabenR v, AufgabenAktuellR v, AufgabeAnlegenR v]
      gruppe g = [GruppeR g]
      aufgabe a = [AufgabeR a, EinsendungAnlegenR a, StatistikR a]
      einsendung a s = [EinsendungR a s]
      servers = [ServersR]
      server s t v k i = concat $ map maybeToList
        [ServerR <$> s
        ,AufgabeVorlagenR <$> s <*> t
        ,AufgabeVorlageR <$> s <*> t <*> v
        ,AufgabeKonfigurationR <$> s <*> t <*> k
        ,AufgabeBenutzerIdR <$> s <*> t <*> k
        ,AufgabeBenutzerIdZufallR <$> s <*> t <*> k
        ,AufgabeTestenR <$> s <*> t <*> k <*> i]
      (as, at, av, ak, ai) = serverRouteInformation mroute
  (schu, sem, vorl, grup, aufg, stud) <- routeBrotkrumen mroute authId
  let server' = server as at av ak ai
      filterRoute r = do hatZugriff <- istAutorisiert authId r
                         return $ maybe False id hatZugriff
      filterBerechtigte =
        (filterM filterRoute) . concat . maybeToList
      zuLink r = Link r $ routeLinkTitel r
      trennstrich [] b = b
      trennstrich a [] = a
      trennstrich a b = a ++ [Trennstrich] ++ b
      addTitel _ [] = []
      addTitel Nothing _ = []
      addTitel t l = map Titel (maybeToList t) ++ l
  schule' <- filterBerechtigte $ schule <$> schu
  schulen' <- filterM filterRoute schulen
  semester' <- filterBerechtigte $ semester <$> sem
  vorlesung' <- filterBerechtigte $ vorlesung <$> vorl
  gruppe' <- filterBerechtigte $ gruppe <$> grup
  aufgabe' <- filterBerechtigte $ aufgabe <$> aufg
  einsendung' <- filterBerechtigte $ einsendung <$> aufg <*> stud
  schuName <- schuleName' schu
  semName <- semesterName' sem
  vorlName <- vorlesungName' vorl
  grupName <- liftIO $ gruppeName grup
  aufgName <- liftIO $ aufgabeName aufg
  return [NavigationMenu MsgSchule $ trennstrich (addTitel schuName $ map zuLink schule') $ map zuLink schulen'
         ,NavigationMenu MsgSemester $ addTitel semName $ map zuLink semester'
         ,NavigationMenu MsgVorlesung $ addTitel vorlName $ map zuLink vorlesung'
         ,NavigationMenu MsgGruppe $ addTitel grupName $ map zuLink gruppe'
         ,NavigationMenu MsgAufgabe $ trennstrich (addTitel aufgName $ map zuLink aufgabe') $ trennstrich (map zuLink einsendung') $ map zuLink $ servers ++ server']

-- | Liefert den Schulnamen zur Id der Schule
schuleName' :: Maybe SchuleId -> Handler (Maybe Text)
schuleName' mschuleId = runMaybeT $ do
  schuleId <- MaybeT . return $ mschuleId
  mschule <- lift $ runDB $ get schuleId
  schule <- MaybeT . return $ mschule
  return $ schuleName schule

-- | Liefert den Semesternamen zur Id des Semesters
semesterName' :: Maybe SemesterId -> Handler (Maybe Text)
semesterName' msemesterId = runMaybeT $ do
  semesterId <- MaybeT . return $ msemesterId
  msemester <- lift $ runDB $ get semesterId
  semester <- MaybeT . return $ msemester
  return $ semesterName semester

-- | Liefert den Vorlesungsnamen zur Id der Vorlesung
vorlesungName' :: Maybe VorlesungId -> Handler (Maybe Text)
vorlesungName' mvorlesungId = runMaybeT $ do
  vorlesungId <- MaybeT . return $ mvorlesungId
  mvorlesung <- lift $ runDB $ get vorlesungId
  vorlesung <- MaybeT . return $ mvorlesung
  return $ vorlesungName vorlesung

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

-- | Liefert die für die Navigation notwendigen Ids der Datenbankeinträge, abhängig von der angegebenen Route. Maybe (Route Autotool), weil Nothing für den Fall einer fehlerhaften URL vorgesehen ist.
routeBrotkrumen :: Maybe (Route Autotool) -> Maybe Int -> Handler (Maybe SchuleId, Maybe SemesterId, Maybe VorlesungId, Maybe GruppeId, Maybe AufgabeId, Maybe StudentId)
routeBrotkrumen Nothing _ = return (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
routeBrotkrumen (Just route) _ = routeInformation route

-- | Liefert die für die Navigation notwendigen Ids der Datenbankeinträge abhängig von der angegebenen Route und unabhängig von der Relevanz für den Nutzer. Dient als Hilfsfunktion für @routeBrotkrumen@.
routeInformation :: Route Autotool -> Handler (Maybe SchuleId, Maybe SemesterId, Maybe VorlesungId, Maybe GruppeId, Maybe AufgabeId, Maybe StudentId)
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
       vorlesung <- liftIO $ getVorlesung $ Just gruppe
       semester <- getSemester vorlesung
       schule <- getSchule semester
       return $ fromTuple6 $ toTuple6 (schule, semester, vorlesung, Just gruppe, Nothing, Nothing)
     AufgabeRoute aufgabe -> do
       vorlesung <- liftIO $ getVorlesungAufgabe $ Just aufgabe
       semester <- getSemester vorlesung
       schule <- getSchule semester
       case fromTuple6 $ toTuple6 (schule, semester, vorlesung, Nothing, Nothing, Nothing) of
         (Just _, Just _, Just _, _,_,_) -> return (schule, semester, vorlesung, Nothing, Just aufgabe, Nothing)
         _ -> return $ fromTuple6 Tuple6_0
     ServerRoute _ -> return $ fromTuple6 Tuple6_0
     EinsendungRoute aufgabe student -> do
       vorlesung <- liftIO $ getVorlesungAufgabe $ Just aufgabe
       semester <- getSemester vorlesung
       schule <- getSchule semester
       case fromTuple6 $ toTuple6 (schule, semester, vorlesung, Nothing, Nothing, Nothing) of
         (Just _, Just _, Just _, _,_,_) -> return (schule, semester, vorlesung, Nothing, Just aufgabe, Just student)
         _ -> return $ fromTuple6 Tuple6_0

-- | Liefert für die Interaktion mit dem Semantik-Server die aktuell abrufbaren Parameter aus der angegebenen Route.
serverRouteInformation :: Maybe (Route Autotool) -> (Maybe ServerUrl, Maybe AufgabeTyp, Maybe VorlageName, Maybe AufgabeKonfiguration, Maybe Text)
serverRouteInformation mroute = case mroute of
  Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing)
  Just route -> case routeParameter route of
    Just (ServerRoute param) ->  case param of
      ServerUrlRoute s -> (Just s, Nothing, Nothing, Nothing, Nothing)
      VorlagenRoute s t -> (Just s, Just t, Nothing, Nothing, Nothing)
      VorlageRoute s t v -> (Just s, Just t, Just v, Nothing, Nothing)
      KonfigurationRoute s t k -> (Just s, Just t, Nothing, Just k, Nothing)
      TestenRoute s t k i -> (Just s, Just t, Nothing, Just k, Just i)
    _ -> (Nothing, Nothing, Nothing, Nothing, Nothing)

-- | Liefert ggf. die Schule zu einem Semester
getSchule :: Maybe SemesterId -> Handler (Maybe SchuleId)
getSchule msemesterId = runMaybeT $ do
  semesterId <- MaybeT $ return msemesterId
  msemester <- lift $ runDB $ get semesterId
  semester <- MaybeT . return $ msemester
  return $ semesterSchuleId semester

-- | Liefert ggf. das Semester zu einer Vorlesung
getSemester :: Maybe VorlesungId -> Handler (Maybe SemesterId)
getSemester mvorlesungId = runMaybeT $ do 
  vorlesungId <- MaybeT $ return mvorlesungId
  mvorlesung <- lift $ runDB $ get vorlesungId
  vorlesung <- MaybeT . return $ mvorlesung
  return $ vorlesungSemesterId vorlesung

-- | Liefert ggf. die Vorlesung zu einer Gruppe
getVorlesung :: Maybe GruppeId -> IO (Maybe VorlesungId)
getVorlesung mgruppe  = runMaybeT $ do
  gruppe <- MaybeT $ return mgruppe
  gruppen <- lift $ GruppeDB.get_gnr $ GNr gruppe
  VNr vorlesungId <- MaybeT $ return $ listToMaybe $ map Gruppe.vnr gruppen
  return $ intToKey vorlesungId

-- | Liefert ggf. die Vorlesung zu einer Aufgabe
getVorlesungAufgabe :: Maybe AufgabeId -> IO (Maybe VorlesungId)
getVorlesungAufgabe maufgabe  = runMaybeT $ do
  aufgabe <- MaybeT $ return maufgabe
  aufgaben <- lift $ AufgabeDB.get_this $ ANr aufgabe
  VNr vorlesungId <- MaybeT $ return $ listToMaybe $ map Aufgabe.vnr aufgaben
  return $ intToKey vorlesungId

data RouteParameter =
    SchuleRoute SchuleId
  | SemesterRoute SemesterId
  | VorlesungRoute VorlesungId
  | GruppeRoute GruppeId
  | AufgabeRoute AufgabeId
  | EinsendungRoute AufgabeId StudentId
  | ServerRoute ServerParameter

data ServerParameter =
   ServerUrlRoute ServerUrl
 | VorlagenRoute ServerUrl AufgabeTyp
 | VorlageRoute ServerUrl AufgabeTyp VorlageName
 | KonfigurationRoute ServerUrl AufgabeTyp AufgabeKonfiguration
 | TestenRoute ServerUrl AufgabeTyp AufgabeKonfiguration Text

-- | Liefert den Parameter, der der Route übergeben wurde
-- 
-- Diese Methode nutzt absichtlich keinen Wildcard-Eintrag, um Rückmeldung vom Compiler bei neu definierten Routen zu erhalten
routeParameter :: Route Autotool -> Maybe RouteParameter
routeParameter route = case route of
  StaticR _                      -> Nothing
  FaviconR                       -> Nothing
  RobotsR                        -> Nothing
  AuthR _                        -> Nothing
  HomeR                          -> Nothing
  SchuleAnlegenR                 -> Nothing
  SchulenR                       -> Nothing
  SchuleR s                      -> Just $ SchuleRoute s
  DirektorenR s                  -> Just $ SchuleRoute s
  DirektorErnennenR s            -> Just $ SchuleRoute s
  WaisenkinderR s                -> Just $ SchuleRoute s
  SemestersR s                   -> Just $ SchuleRoute s
  SemesterAnlegenR s             -> Just $ SchuleRoute s
  SemesterR s                    -> Just $ SemesterRoute s
  VorlesungenR s                 -> Just $ SemesterRoute s
  VorlesungAnlegenR s            -> Just $ SemesterRoute s
  VorlesungR v                   -> Just $ VorlesungRoute v
  GruppeAnlegenR v               -> Just $ VorlesungRoute v
  GruppenR v                     -> Just $ VorlesungRoute v
  StudentenR v                   -> Just $ VorlesungRoute v
  TutorErnennenR v               -> Just $ VorlesungRoute v
  TutorenR v                     -> Just $ VorlesungRoute v
  ResultateR v                   -> Just $ VorlesungRoute v
  ResultatePflichtR v            -> Just $ VorlesungRoute v
  AufgabeAnlegenR v              -> Just $ VorlesungRoute v
  AufgabenR v                    -> Just $ VorlesungRoute v
  AufgabenAktuellR v             -> Just $ VorlesungRoute v
  GruppeR g                      -> Just $ GruppeRoute g
  AufgabeR a                     -> Just $ AufgabeRoute a
  EinsendungAnlegenR a           -> Just $ AufgabeRoute a
  StatistikR a                   -> Just $ AufgabeRoute a
  EinsendungR a s                -> Just $ EinsendungRoute a s
  ServersR                       -> Nothing
  ServerR s                      -> Just $ ServerRoute $ ServerUrlRoute s
  AufgabeVorlagenR s t           -> Just $ ServerRoute $ VorlagenRoute s t
  AufgabeVorlageR s t v          -> Just $ ServerRoute $ VorlageRoute s t v
  AufgabeKonfigurationR s t k    -> Just $ ServerRoute $ KonfigurationRoute s t k
  AufgabeBenutzerIdR s t k       -> Just $ ServerRoute $ KonfigurationRoute s t k
  AufgabeBenutzerIdZufallR s t k -> Just $ ServerRoute $ KonfigurationRoute s t k
  AufgabeTestenR s t k i         -> Just $ ServerRoute $ TestenRoute s t k i

-- | Liefert den Titel, der für einen Link zu dieser Route verwendet werden soll
-- 
-- Diese Methode nutzt absichtlich keinen Wildcard-Eintrag, um Rückmeldung vom Compiler bei neu definierten Routen zu erhalten
routeLinkTitel :: Route Autotool -> Maybe AutotoolMessage
routeLinkTitel route = case route of
  StaticR _                      -> Nothing
  FaviconR                       -> Nothing
  RobotsR                        -> Nothing
  AuthR _                        -> Nothing
  HomeR                          -> Nothing
  SchuleAnlegenR                 -> Just MsgSchuleAnlegen
  SchulenR                       -> Just MsgSchulen
  SchuleR _                      -> Just MsgBearbeiten
  DirektorenR _                  -> Just MsgDirektoren
  DirektorErnennenR _            -> Just MsgDirektorErnennen
  WaisenkinderR _                -> Just MsgWaisenkinder
  SemestersR _                   -> Just MsgSemesters
  SemesterAnlegenR _             -> Just MsgSemesterAnlegen
  SemesterR _                    -> Just MsgBearbeiten
  VorlesungenR _                 -> Just MsgVorlesungen
  VorlesungAnlegenR _            -> Just MsgVorlesungAnlegen
  VorlesungR _                   -> Just MsgBearbeiten
  GruppeAnlegenR _               -> Just MsgGruppeAnlegen
  GruppenR _                     -> Just MsgGruppen
  StudentenR _                   -> Just MsgStudenten
  TutorErnennenR _               -> Just MsgTutor
  TutorenR _                     -> Just MsgTutoren
  ResultateR _                   -> Just MsgAlleResultate
  ResultatePflichtR _            -> Just MsgResultatePflicht
  AufgabeAnlegenR _              -> Just MsgAufgabeAnlegen
  AufgabenR _                    -> Just MsgAufgaben
  AufgabenAktuellR _             -> Just MsgAufgabenAktuell
  GruppeR _                      -> Just MsgBearbeiten
  AufgabeR _                     -> Just MsgBearbeiten
  EinsendungAnlegenR _           -> Just MsgLösen
  StatistikR _                   -> Just MsgStatistikAnzeigen
  EinsendungR _ _                -> Just MsgEinsendungAnzeigen
  ServersR                       -> Just MsgServers
  ServerR _                      -> Just MsgAufgabeTyp
  AufgabeVorlagenR _ _           -> Just MsgAufgabeVorlagen
  AufgabeVorlageR _ _ _          -> Just MsgAufgabeKonfigurieren
  AufgabeKonfigurationR _ _ _    -> Just MsgAufgabeKonfigurieren
  AufgabeBenutzerIdR _ _ _       -> Just MsgAufgabeBenutzerId
  AufgabeBenutzerIdZufallR _ _ _ -> Just MsgAufgabeBenutzerIdZufall
  AufgabeTestenR _ _ _ _         -> Just MsgAufgabeTesten

-- | Liefert den Titel, der bei dieser Route verwendet werden soll.
-- 
-- Diese Methode nutzt absichtlich keinen Wildcard-Eintrag, um Rückmeldung vom Compiler bei neu definierten Routen zu erhalten
routeTitel :: Route Autotool -> Handler (Maybe AutotoolMessage)
routeTitel route = case route of
  StaticR _                      -> return Nothing
  FaviconR                       -> return Nothing
  RobotsR                        -> return Nothing
  AuthR _                        -> return Nothing
  HomeR                          -> return $ Just MsgAutotool
  SchuleAnlegenR                 -> return $ Just MsgSchuleAnlegen
  SchulenR                       -> return $ Just MsgSchulen
  SchuleR _                      -> return $ Just MsgSchuleBearbeiten
  DirektorenR _                  -> return $ Just MsgDirektoren
  DirektorErnennenR _            -> return $ Just MsgDirektorErnennen
  WaisenkinderR _                -> return $ Just MsgWaisenkinder
  SemestersR _                   -> return $ Just MsgSemesters
  SemesterAnlegenR _             -> return $ Just MsgSemesterAnlegen
  SemesterR _                    -> return $ Just MsgSemesterBearbeiten
  VorlesungenR _                 -> return $ Just MsgVorlesungen
  VorlesungAnlegenR _            -> return $ Just MsgVorlesungAnlegen
  VorlesungR _                   -> return $ Just MsgVorlesungBearbeiten
  GruppeAnlegenR _               -> return $ Just MsgGruppeAnlegen
  GruppenR _                     -> return $ Just MsgGruppen
  GruppeR _                      -> return $ Just MsgGruppeBearbeiten
  TutorErnennenR _               -> return $ Just MsgTutor
  TutorenR _                     -> return $ Just MsgTutoren
  StudentenR _                   -> return $ Just MsgStudenten
  ResultatePflichtR _            -> return $ Just MsgResultatePflicht
  ResultateR _                   -> return $ Just MsgAlleResultate
  AufgabeAnlegenR _              -> return $ Just MsgAufgabeAnlegen
  AufgabenR _                    -> return $ Just MsgAufgabenAlle
  AufgabenAktuellR _             -> return $ Just MsgAufgabenAktuell
  AufgabeR _                     -> return $ Just MsgAufgabeBearbeiten
  StatistikR _                   -> return $ Just MsgStatistik
  EinsendungR _ _                -> return $ Just MsgEinsendung
  EinsendungAnlegenR a           -> do
    maufgabe <- lift $ liftM listToMaybe $ AufgabeDB.get_this $ ANr a
    case maufgabe of
      Just aufgabe -> return $ Just $ MsgAufgabeXLösen $ pack $ toString $ Aufgabe.name aufgabe
      Nothing -> notFound
  ServersR                       -> return $ Just MsgServers
  ServerR _                      -> return $ Just MsgAufgabeTyp
  AufgabeVorlagenR _ _           -> return $ Just MsgAufgabeVorlagen
  AufgabeVorlageR _ _ _          -> return $ Just MsgAufgabeKonfigurieren
  AufgabeKonfigurationR _ _ _    -> return $ Just MsgAufgabeKonfigurieren
  AufgabeBenutzerIdR _ _ _       -> return $ Just MsgAufgabeBenutzerId
  AufgabeBenutzerIdZufallR _ _ _ -> return $ Just MsgAufgabeBenutzerIdZufall
  AufgabeTestenR _ t _ _         -> return $ Just $ MsgAufgabeXTesten t
