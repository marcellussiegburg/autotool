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

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Foldable (foldlM)
import Data.Maybe (listToMaybe)
import Data.Set (member)
import Data.Text (Text)
import Data.Text.Read (decimal, signed)
import Data.Time (UTCTime, formatTime)
import Data.Tuple6
import System.Locale (defaultTimeLocale)
import Model

import Control.Types
import qualified Control.Admin.DB as AdminDB
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
            let navigation = $(widgetFile "navigation")
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized route _
      | "student" `member` routeAttrs route = do
          mid <- maybeAuthId
          return $ maybe AuthenticationRequired (\_ -> Authorized) mid
      | "admin" `member` routeAttrs route =
          autorisierungChecken route
      | "direktor" `member` routeAttrs route =
          autorisierungChecken route
      | "tutor" `member` routeAttrs route =
          autorisierungChecken route
      | otherwise = return Authorized

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

instance YesodJquery Autotool where
    urlJqueryJs _ = Left $ StaticR js_jquery_min_js
    urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_min_js
    urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_min_css
    urlJqueryUiDateTimePicker _ = Left $ StaticR js_jquery_ui_datetimepicker_js

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
    authPlugins _ = [authAutotool $ Nothing] --authBrowserId def]
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

autorisierungChecken :: Route Autotool -> Handler AuthResult
autorisierungChecken route = do
  mid <- maybeAuthId
  case mid of
    Nothing -> return AuthenticationRequired
    Just aId -> let braucht a = if a `member` routeAttrs route then [a] else []
                    attrs = concat [braucht "admin", braucht "direktor", braucht "tutor"]
                in foldlM (\c a -> if c == Authorized then return Authorized else (checke a aId route)) (AuthenticationRequired) attrs

checke :: Text -> Int -> Route Autotool -> Handler AuthResult
checke rolle authId route
  | rolle == "admin" = do
      mstud <- lift $ StudDB.get_snr (SNr authId)
      case mstud of
        [stud] -> do
          isAdmin <- lift $ AdminDB.is_minister stud
          if isAdmin
             then return Authorized
             else unauthorizedI MsgNichtAutorisiert
        _ -> unauthorizedI MsgNichtAutorisiert
  | rolle == "direktor" = do
      mstud <- lift $ StudDB.get_snr (SNr authId)
      case mstud of
        [stud] -> do
          schulen <- lift $ DirektorDB.get_directed stud
          (mschule,_,_,_,_,_) <- lift $ routeInformation route
          case mschule of
            Nothing -> unauthorizedI MsgNichtAutorisiert
            Just schule ->
              if elem (UNr schule) $ map (Schule.unr) schulen
              then return Authorized
              else unauthorizedI MsgNichtAutorisiert
        _ -> unauthorizedI MsgNichtAutorisiert
  | rolle == "tutor" = do
      mstud <- lift $ StudDB.get_snr (SNr authId)
      case mstud of
        [stud] -> do
          vorlesungen <- liftIO $ TutorDB.get_tutored stud
          (_,_,mvorlesung,_,_,_) <- lift $ routeInformation route
          case mvorlesung of
            Nothing -> unauthorizedI MsgNichtAutorisiert
            Just vorlesung ->
              if elem (VNr vorlesung) $ map (Vorlesung.vnr) vorlesungen
              then return Authorized
              else unauthorizedI MsgNichtAutorisiert
        _ -> unauthorizedI MsgNichtAutorisiert
  | otherwise =
      unauthorizedI MsgNichtAutorisiert

routeInformation :: Route Autotool -> IO (Maybe SchuleId, Maybe SemesterId, Maybe VorlesungId, Maybe [GruppeId], Maybe AufgabeId, Maybe Text)
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
       gruppe <- getGruppe $ Just vorlesung
       return $ fromTuple6 $ toTuple6 (schule, semester, Just vorlesung, gruppe, Nothing, Nothing)
     GruppeRoute gruppe -> do
       vorlesung <- getVorlesung $ Just gruppe
       semester <- getSemester vorlesung
       schule <- getSchule semester
       return $ fromTuple6 $ toTuple6 (schule, semester, vorlesung, Just [gruppe], Nothing, Nothing)
     AufgabeRoute aufgabe -> do
       vorlesung <- getVorlesungAufgabe $ Just aufgabe
       gruppe <- getGruppe vorlesung
       semester <- getSemester vorlesung
       schule <- getSchule semester
       return $ fromTuple6 $ toTuple6 (schule, semester, vorlesung, gruppe, Just aufgabe, Nothing)

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
getVorlesungAufgabe mgruppe  = runMaybeT $ do
  gruppe <- MaybeT $ return mgruppe
  gruppen <- lift $ GruppeDB.get_gnr $ GNr gruppe
  VNr vorlesungId <- MaybeT $ return $ listToMaybe $ map Gruppe.vnr gruppen
  return vorlesungId

-- | Liefert ggf. die Gruppen zu einer Vorlesung
getGruppe :: Maybe VorlesungId -> IO (Maybe [GruppeId])
getGruppe mvorlesung = runMaybeT $ do
  vorlesung <- MaybeT $ return mvorlesung
  gruppen <- lift $ GruppeDB.get_this $ VNr vorlesung
  return $ map (\g -> let GNr gId = Gruppe.gnr g in gId) gruppen

-- | Liefert ggf. eine Gruppe zu einer Vorlesung, die der Student besucht
getBesuchteGruppe :: Maybe VorlesungId -> Maybe Int -> IO (Maybe GruppeId)
getBesuchteGruppe mvorlesung mauthId = runMaybeT $ do
  vorlesung <- MaybeT $ return mvorlesung
  authId <- MaybeT $ return mauthId
  gruppen <- lift $ GruppeDB.get_attended (VNr vorlesung) $ SNr authId
  GNr gruppeId <- MaybeT $ return $ listToMaybe $ map Gruppe.gnr gruppen
  return gruppeId

data RouteParameter = SchuleRoute SchuleId | SemesterRoute SemesterId | VorlesungRoute VorlesungId | GruppeRoute GruppeId | AufgabeRoute AufgabeId

-- | Liefert den Parameter, der der Route übergeben wurde
-- 
-- Diese Methode nutzt absichtlich kein Wildcard-Eintrag, um Rückmeldung vom Compiler bei neu definierten Routen zu erhalten
routeParameter :: Route Autotool -> Maybe RouteParameter
routeParameter route = case route of
  StaticR _                   -> Nothing
  FaviconR                    -> Nothing
  RobotsR                     -> Nothing
  AuthR _                     -> Nothing
  HomeR                       -> Nothing
  SchuleAnlegenR              -> Nothing
  SchulenR                    -> Nothing
  SchuleR a                   -> Just $ SchuleRoute a
  DirektorenR a               -> Just $ SchuleRoute a
  DirektorErnennenR a         -> Just $ SchuleRoute a
  WaisenkinderR a             -> Just $ SchuleRoute a
  SemestersR a                -> Just $ SchuleRoute a
  SemesterAnlegenR a          -> Just $ SchuleRoute a
  SemesterR a                 -> Just $ SemesterRoute a
  VorlesungenR a              -> Just $ SemesterRoute a
  VorlesungAnlegenR a         -> Just $ SemesterRoute a
  VorlesungR a                -> Just $ VorlesungRoute a
  GruppeAnlegenR a            -> Just $ VorlesungRoute a
  GruppenR a                  -> Just $ VorlesungRoute a
  StudentenR a                -> Just $ VorlesungRoute a
  TutorErnennenR a            -> Just $ VorlesungRoute a
  TutorenR a                  -> Just $ VorlesungRoute a
  ResultateR a                -> Just $ VorlesungRoute a
  ResultatePflichtR a         -> Just $ VorlesungRoute a
  GruppeR a                   -> Just $ GruppeRoute a
  AufgabeAnlegenR a           -> Just $ GruppeRoute a
  AufgabenR a                 -> Just $ GruppeRoute a
  AufgabenAktuellR a          -> Just $ GruppeRoute a
  AufgabeBearbeitenR a        -> Just $ AufgabeRoute a
  AufgabeR a                  -> Just $ AufgabeRoute a
  StatistikR a                -> Just $ AufgabeRoute a
  ServersR                    -> Nothing
  ServerR _                   -> Nothing
  AufgabeVorlagenR _ _        -> Nothing
  AufgabeVorlageR _ _ _       -> Nothing
  AufgabeKonfigurationR _ _ _ -> Nothing
  AufgabeTestenR _ _ _        -> Nothing
