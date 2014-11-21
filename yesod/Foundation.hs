module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)

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
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
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
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod
