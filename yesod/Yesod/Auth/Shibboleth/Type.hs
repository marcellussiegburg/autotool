{-# LANGUAGE NoImplicitPrelude #-}
module Yesod.Auth.Shibboleth.Type where

import Data.Maybe (Maybe)
import Data.Text (Text)
import System.IO (IO)
import Yesod.Core.Types (HandlerT)

import Yesod.Auth
import Model
import Control.Student.Type (Student)

class YesodAuth site => YesodAuthShibboleth site where
  -- ^ Parameter: Schoolname
  getSchoolByName :: Text -> HandlerT site IO SchuleId
  -- ^ Parameter: Schoolname, Name, Surname, matrikel
  getAccountByName :: SchuleId -> Text -> Text -> Text  -> HandlerT site IO (Maybe StudentId)
  getAccountByEppn :: SchuleId -> Text -> HandlerT site IO (Maybe StudentId)
