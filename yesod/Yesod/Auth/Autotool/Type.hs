{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Yesod.Auth.Autotool.Type
    (module Yesod.Auth.Autotool.Type,
     module Yesod.Auth.Autotool.Message
    ) where

import Yesod.Auth.Autotool.Message

import Yesod.Auth
import Yesod.Core

import Control.Monad
import Data.Int
import Data.Maybe
import System.IO

import Control.Student.Type
import qualified Control.Schule.Typ as Schule

import Control.Types

class YesodAuth site => YesodAuthAutotool site where
    type AuthSchool
    type AuthStudent

    getStudentByMNr :: MonadIO (a site IO) =>
                       UNr -> MNr -> a site IO [Student]
    getStudentByAuthStudent ::
        (Monad (a site IO), MonadIO (a site IO)) =>
        AuthSchool -> AuthStudent -> a site IO [Student]
    getSchools :: (Monad (a site IO), MonadIO (a site IO)) =>
                  a site IO [Schule.Schule]
    getSchool :: (Monad (a site IO), MonadIO (a site IO)) =>
                 Maybe AuthSchool -> a site IO (Maybe Schule.Schule)
    toSchool :: (Monad (a site IO), MonadIO (a site IO)) =>
                Int -> a site IO (Maybe Schule.Schule)
    studentToAuthStudent :: Monad (a site IO) => Student -> a site IO AuthStudent
    schoolToAuthSchool :: Monad (a site IO) => Schule.Schule -> a site IO AuthSchool

signinR :: AuthRoute
signinR = PluginR "autotool" ["login"]

signinSchoolR :: PathPiece AuthSchool => AuthSchool -> AuthRoute
signinSchoolR u = PluginR "autotool" ["login", toPathPiece u]

loginR :: PathPiece AuthSchool => Maybe AuthSchool -> AuthRoute
loginR u = case u of
             Nothing -> signinR
             Just u' -> signinSchoolR u'

createAccountR :: AuthRoute
createAccountR = PluginR "autotool" ["create"]

createAccountSchoolR :: PathPiece AuthSchool => AuthSchool -> AuthRoute
createAccountSchoolR u = PluginR "autotool" ["create", toPathPiece u]

resetPasswordR ::
    (PathPiece AuthSchool, PathPiece AuthStudent) =>
    AuthSchool -> AuthStudent -> AuthRoute
resetPasswordR u m = PluginR "autotool" ["reset", toPathPiece u, toPathPiece m]
{-
modifyR :: AuthSchool -> AuthRoute
modifyR unr = PluginR "autotool" ["modify", toText unr]
-}
