{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Autotool
    (module Yesod.Auth.Autotool,
     module Yesod.Auth.Autotool.Type
    ) where

import Yesod.Auth.Autotool.Type
import Yesod.Auth.Autotool.Widget

import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Core
import Yesod.Form.Input
import Yesod.Form.Fields

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Bool
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Either
import Data.Int
import Control.Applicative
import Control.Monad
import Network.Mail.Mime
import System.IO
import Text.Email.Validate
import Text.Show
import Prelude (undefined)

import Control.Student.Type as Stud
import qualified Control.Student.DB as StudDB
import qualified Control.Schule.Typ as Schule
import Control.Types
import qualified Operate.Crypt as C
import qualified Control.Student.CGI as CGI

fromPP :: (PathPiece p, MonadHandler m) => T.Text -> (p -> m a) -> m a
fromPP p m = maybe notFound (\ p' -> m p') $ fromPathPiece p

authAutotool ::
    (YesodAuthAutotool m, PathPiece AuthSchool,
     PathPiece AuthStudent, ToString AuthStudent,
     RenderMessage m YesodAuthAutotoolMessage) =>
    Maybe AuthSchool -> AuthPlugin m
authAutotool u = AuthPlugin "autotool" dispatch $ \tm -> do
      authAutotoolWidget u Nothing Nothing Nothing tm
    where dispatch "GET"  ["login"] =
              getSigninR >>= sendResponse
          dispatch "POST" ["login"] = 
              postSigninR >>= sendResponse
          dispatch "GET"  ["login", u'] = fromPP u' $ \u'' ->
              getSigninSchoolR u'' >>= sendResponse
          dispatch "POST" ["login", u'] = fromPP u' $ \u'' ->
              postSigninSchoolR u'' >>= sendResponse
          dispatch "GET"  ["create"] =
              getCreateAccountR >>= sendResponse
          dispatch "POST" ["create"] = 
              postCreateAccountR >>= sendResponse
          dispatch "GET"  ["create", u'] = fromPP u' $ \u'' ->
              getCreateAccountSchoolR u'' >>= sendResponse
          dispatch "POST" ["create", u'] = fromPP u' $ \u'' ->
              postCreateAccountSchoolR u'' >>= sendResponse
          dispatch "GET"  ["reset", u', m] =
              fromPP u' $ \u'' ->
                  fromPP m $ \m' ->
                      getResetPasswordR u'' m' >>= sendResponse
          dispatch "POST" ["reset", u', m] =
              fromPP u' $ \u'' ->
                  fromPP m $ \m' ->
                      postResetPasswordR u'' m' >>= sendResponse
--          dispatch "GET"  ["verify", u, k] = getVerifyR u k >>= sendResponse
--          dispatch "GET"  ["newpassword", u, k] = getNewPasswordR u k >>= sendResponse
{-          dispatch "GET"  ["edit"] = getEditAccountR >>= sendResponse
          dispatch "POST" ["edit"] = postEditAccountR >>= sendResponse-}
--          dispatch "POST" ["resendverifyemail"] = postResendVerifyEmailR >>= sendResponse
          dispatch _ _ = notFound

data LoginData = LoginData (Maybe Int) T.Text T.Text

postLoginR ::
    (YesodAuthAutotool master,
     PathPiece AuthStudent, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    Maybe AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
postLoginR = postLogin

postLogin ::
    (YesodAuthAutotool master,
     PathPiece AuthStudent, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    Maybe AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
postLogin u = do
  LoginData school identifier pass <- lift $ runInputPost $ LoginData
        <$> iopt intField "school" 
        <*> ireq textField "matriculationnumber"
        <*> ireq textField "password"
  u' <- case school of
          Just s -> lift $ toSchool s
          Nothing -> lift $ getSchool u
  case u' of
    Nothing ->
      lift $ selectRep $
        provideRep $ defaultLayout $
          errorWidget
    Just u'' -> do
        studs <- lift $ getStudentByMNr (Schule.unr u'') (MNr $ T.unpack identifier)
        (mstud,wrongPass) <- case studs of
            [stud] | C.compare (passwort stud) $ T.unpack pass -> do
                       s <- currentPasswordUsed stud
                       return (s, Nothing)
                   | C.compare (next_passwort stud) $ T.unpack pass -> do
                       s <- newPasswordUsed stud
                       return (s, Nothing)
                   | otherwise ->
                       return (Nothing, Just $ stud)
            _ ->
--              loginErrorMessage (tp $ loginR u) "Matrikelnummer oder Schule falsch."
              return (Nothing, Nothing)
        case mstud of
          Nothing -> --loginErrorMessage (tp $ loginR u) "Unknown error"
              case wrongPass of
              Just s -> do
                tp <- getRouteToParent
                s' <- lift $ studentToAuthStudent s
                u''' <- lift $ schoolToAuthSchool u''
                lift $ selectRep $
                  provideRep $ defaultLayout $ authAutotoolWidget u (Just u''') (Just s') wrongPass tp
              Nothing -> do --loginErrorMessage (tp $ loginR u) "Unknown error"
                tp <- getRouteToParent
                u''' <- lift $ schoolToAuthSchool u''
                loginErrorMessageI (loginR u) InvalidLogin --"Unknown error"
                lift $ selectRep $
                  provideRep $ defaultLayout $
                    authAutotoolWidget u (Just u''') Nothing wrongPass tp
                    --errorWidget
          Just stud ->
              let SNr s = snr stud
              in lift $ setCredsRedirect $ Creds "autotool" (T.pack $ show $ s) []

getSignin :: 
    (YesodAuthAutotool master,
     PathPiece AuthStudent, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    Maybe AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
getSignin u = do
  tp <- getRouteToParent
  lift $ selectRep $
    provideRep $ defaultLayout $ authAutotoolWidget u Nothing Nothing Nothing tp

getSigninR :: 
    (YesodAuthAutotool master,
     PathPiece AuthStudent, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    HandlerT Auth (HandlerT master IO) TypedContent
getSigninR = getSignin Nothing

getSigninSchoolR :: 
    (YesodAuthAutotool master,
     PathPiece AuthStudent, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
getSigninSchoolR u = getSignin (Just u)

postSigninR ::
    (YesodAuthAutotool master,
     PathPiece AuthStudent, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    HandlerT Auth (HandlerT master IO) TypedContent
postSigninR = postLogin Nothing

postSigninSchoolR ::
    (YesodAuthAutotool master,
     PathPiece AuthStudent, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
postSigninSchoolR u = postLogin $ Just u

currentPasswordUsed :: MonadIO m => Student -> m (Maybe Student)
currentPasswordUsed stud =
    if C.is_empty $ next_passwort stud
    then return $ Just stud
    else do {-
    [ "Sie hatten eine Email mit einem neuen Passwort erhalten,"
    , "aber Sie haben jetzt Ihr altes Passwort benutzt."
    , "Das Passwort aus der Email wird dadurch ungültig,"
    , "Ihr bestehendes (jetzt benutztes) Passwort bleibt gültig."
    ]-}
      let neu = stud { next_passwort = C.empty }
      liftIO $ StudDB.put (Just $ snr stud) neu
      return $ Just neu

newPasswordUsed :: MonadIO m => Student -> m (Maybe Student)
newPasswordUsed stud = do {-
  "Sie haben Ihr neues Passwort verwendet."
  "Das vorherige ist damit ungültig."
                           -}
  let neu = stud { passwort = next_passwort stud
                 , next_passwort = C.empty
                 }
  liftIO $ StudDB.put (Just $ snr stud) neu
  return $ Just neu

getResetPasswordR ::
    (YesodAuthAutotool master, PathPiece AuthSchool,
     PathPiece AuthStudent,
     RenderMessage master YesodAuthAutotoolMessage) =>
    AuthSchool
    -> AuthStudent
    -> HandlerT Auth (HandlerT master IO) TypedContent
getResetPasswordR u m = studentExistsOnce u m $ \ _ -> do
  tp <- getRouteToParent
  lift $ selectRep $
    provideRep $ defaultLayout $ do
      resetPasswordWidget u m tp

postResetPasswordR ::
    (YesodAuthAutotool master, ToString AuthStudent,
     RenderMessage master YesodAuthAutotoolMessage) =>
    AuthSchool
    -> AuthStudent
    -> HandlerT Auth (HandlerT master IO) TypedContent
postResetPasswordR u m = studentExistsOnce u m $ \ stud ->
   case validate (encodeUtf8 $ toText $ email stud) of
    Left _ ->
--      tp <- getRouteToParent
--      loginErrorMessage (tp $ loginR unr) $ toText err
      lift $ selectRep $
        provideRep $ defaultLayout $ do
          invalidEmailWidget
    Right e -> do
      p <- liftIO CGI.pass
      c <- liftIO $ C.encrypt p
      liftIO $ StudDB.put (Just $ snr stud) $ stud { next_passwort = c }
      let toMail = decodeUtf8With lenientDecode $ toByteString e
          fromMail = "autotool@localhost"
          to = Address { addressName = Just (T.append (toText $ vorname stud) $ toText $ Stud.name stud),
                         addressEmail = toMail }
          from = Address { addressName = Just "Autotool",
                           addressEmail = fromMail }
          text =
              [ "Sie haben ein neues Passwort",
                "für das E-Learning-System autotool angefordert.",
                LT.unwords [ "Es lautet:", "Matrikelnummer:", toLText m, "Passwort:", LT.pack p ],
                "Es wird durch seine erste Benutzung aktiviert,",
                "Sie können es danach ändern.",
                "Sie können aber auch Ihr bisheriges Passwort weiter benutzen",
                "und diese Mail ignorieren." ]
          content = foldl (\a b -> LT.append (LT.append a b) "\n") "" text
          mail = simpleMail' to from "Autotool: Passwortreset" content
      liftIO $ renderSendMail mail
      u' <- lift $ getSchool $ Just u
      lift $ selectRep $
        provideRep $ defaultLayout $ do
          let Just u'' = u'
              Name un = Schule.name u''
          mailSentWidget (T.pack un) $ mnr stud

getCreateAccountR ::
    (YesodAuthAutotool master, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    HandlerT Auth (HandlerT master IO) TypedContent
getCreateAccountR = getCreateAccount Nothing

getCreateAccountSchoolR ::
    (YesodAuthAutotool master, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
getCreateAccountSchoolR u = getCreateAccount (Just u)

getCreateAccount ::
    (YesodAuthAutotool master, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    Maybe AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
getCreateAccount u = do
  tp <- getRouteToParent
  lift $ selectRep $
    provideRep $ defaultLayout $ do
      createAccountWidget u tp

data AccountData = AccountData (Maybe Int) T.Text T.Text T.Text T.Text

postCreateAccount ::
    (YesodAuthAutotool master, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    Maybe AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
postCreateAccount u = do
  AccountData mschool identifier firstname surname email <- lift $ runInputPost $ AccountData
        <$> iopt intField "school" 
        <*> ireq textField "matriculationnumber"
        <*> ireq textField "firstname"
        <*> ireq textField "surname"
        <*> ireq textField "email"
  tp <- getRouteToParent
  lift $ selectRep $
    provideRep $ defaultLayout $ do
      createAccountWidget u tp

postCreateAccountR ::
    (YesodAuthAutotool master, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    HandlerT Auth (HandlerT master IO) TypedContent
postCreateAccountR = postCreateAccount Nothing

postCreateAccountSchoolR :: 
    (YesodAuthAutotool master, PathPiece AuthSchool,
     RenderMessage master YesodAuthAutotoolMessage) =>
    AuthSchool -> HandlerT Auth (HandlerT master IO) TypedContent
postCreateAccountSchoolR u = postCreateAccount $ Just u

studentExistsOnce ::
    YesodAuthAutotool master =>
    AuthSchool
    -> AuthStudent
    -> (Student -> HandlerT Auth (HandlerT master IO) TypedContent)
    -> HandlerT Auth (HandlerT master IO) TypedContent
studentExistsOnce u m h = do
  studs <- lift $ getStudentByAuthStudent u m
  case studs of
    [stud] -> h stud
    _ -> notFound

toText :: ToString a => a -> T.Text
toText = T.pack . toString

toLText :: ToString a => a -> LT.Text
toLText = LT.pack . toString
