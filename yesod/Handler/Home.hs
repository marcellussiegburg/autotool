module Handler.Home where

import Import
import Handler.Schulen (getSchulenR)

getHomeR :: Handler Html
getHomeR = getSchulenR
