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

import Data.Maybe (listToMaybe)
import Data.Time (UTCTime, formatTime)
import Data.Text (Text)
import Data.Text.Read (decimal, signed)
import System.Locale (defaultTimeLocale)
import Model

import Control.Types
import qualified Control.Student.DB as StudDB
import qualified Control.Schule.DB as SchuleDB
import qualified Control.Student.Type as Student
import qualified Control.Schule.Typ as Schule

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

    -- Routes not requiring authenitcation.
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

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
    -- Where to send a user after successful login
    loginDest _ = SchulenR
    -- Where to send a user after logout
    logoutDest _ = SchulenR
    getAuthId creds = liftIO $ do
      case signed decimal $ credsIdent creds of
        Right (s, "") -> do
          x <- StudDB.get_snr $ SNr s
          case x of
            [_] -> return $ Just $ s
            _ -> return $ Nothing
        _ -> return Nothing
{-
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    }-}
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
                  [stud] -> return $ Just s
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
