module Yesod.Auth.Shibboleth
  (module Yesod.Auth.Shibboleth,
   module Yesod.Auth.Shibboleth.Type
  ) where

import Control.Monad ((>>=), return, when)
import Control.Monad.Trans.Class (lift)
import Data.Bool (Bool (False), (||))
import Data.Char (toUpper)
import Data.Eq ((==), (/=))
import Data.Function (($), (.))
import qualified Data.List as L (length)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import System.IO (IO)
import Data.Ord ((<))
import Data.Text (cons, empty, head, length, pack, split, tail)
import Data.Text.Encoding (decodeUtf8)
import Text.Blaze.Html (Html)
import Text.Show (show)
import Yesod.Auth (AuthPlugin (AuthPlugin), AuthRoute, Creds (Creds), Route, YesodAuth, loginDest, setCreds)
import Yesod.Core (getRouteToParent, defaultLayout)
import Yesod.Core.Handler (getYesod, lookupHeader, notFound, provideRep, redirect, selectRep, setMessage)
import Yesod.Core.Widget (WidgetT, handlerToWidget)

import Yesod.Auth.Shibboleth.Type

authShibboleth :: (YesodAuth m, YesodAuthShibboleth m) => AuthPlugin m
authShibboleth = AuthPlugin "shibboleth" dispatch $ \tp -> 
  getLogin tp
  where
    dispatch "GET" ["login"] = getRouteToParent >>= lift . selectRep . provideRep . defaultLayout . getLogin
    dispatch _ _ = notFound

getLogin :: (YesodAuth master, YesodAuthShibboleth master) => (AuthRoute -> Route master) -> WidgetT master IO ()
getLogin _tp = do
  midp <- lookupHeader' "X-Shibboleth-Identity-Provider"
  idp <- maybe (redirectWith "Login Failed. Sure, you successfuly logged in via Shibboleth? Because there was no Identity Provider provided.") return midp
  school <- handlerToWidget $ getSchoolByName $ decodeUtf8 idp
  meppn <- lookupHeader' "X-Shibboleth-Eppn"
  eppn <- maybe getAnonymousLogin return meppn
  --eppn <- maybe (redirectWith "missing shibboleth attribute: eduPersonPrincipalName") return meppn
  let eppn' = split (== '@') $ decodeUtf8 eppn
      [loginname,institute] = eppn'
  when (L.length eppn' /= 2 || length institute < 2) $ redirectWith "Malformed eppn"
  --schulen <- handlerToWidget $ runDB $ selectList [SchuleMailSuffix ==. Just suffix] []
  let vorname = cons (toUpper $ head loginname) empty
      name = cons (toUpper $ head $ tail loginname) $ tail $ tail loginname
  mstudent <- handlerToWidget $ getAccountByName school vorname name loginname
  maffiliation <- lookupHeader' "X-Shibboleth-Affiliation"
  student <- maybe (redirectWith "Login Failed - Could not create Student") return mstudent
  handlerToWidget $ setCreds False $ Creds "shibboleth" (pack $ show student) []
  redirectWith "Logged In"

getAnonymousLogin :: (YesodAuth master) => WidgetT master IO a
getAnonymousLogin = do
  handlerToWidget $ setCreds False $ Creds "shibboleth" "-1" []
  redirectWith "Logged in anonymously"

redirectWith :: (YesodAuth master) => Html -> WidgetT master IO a
redirectWith message = do
  autotool <- getYesod
  setMessage message
  redirect $ loginDest autotool

--lookupHeader' :: CI ByteString -> WidgetT master IO (Maybe ByteString)
lookupHeader' h = do
  h' <- lookupHeader h
  case h' of
    Just "(null)" -> return Nothing
    h'' -> return h''
