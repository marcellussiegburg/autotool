module Handler.Home where

import Import
import Yesod.Auth

getHomeR :: Handler Html
getHomeR =
  defaultLayout
    $(widgetFile "home")
