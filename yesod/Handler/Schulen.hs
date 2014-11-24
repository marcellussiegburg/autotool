module Handler.Schulen where

import Import

getSchulenR :: Handler Html
getSchulenR = do
  defaultLayout $ do
    $(widgetFile "schulen")
