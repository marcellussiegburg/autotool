module Handler.Home where

import Import
import Prelude (undefined)
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
  a <- requireAuthId
  defaultLayout $ [whamlet|foo|]

postHomeR :: Handler Html
postHomeR = undefined
