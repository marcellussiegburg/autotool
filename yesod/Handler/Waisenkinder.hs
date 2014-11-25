module Handler.Waisenkinder where

import Import

getWaisenkinderR :: SchuleId -> Handler Html
getWaisenkinderR schule = do
  defaultLayout $ do
    $(widgetFile "waisenkinder")
