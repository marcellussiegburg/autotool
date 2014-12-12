module Handler.Semesters where

import Import

getSemestersR :: SchuleId -> Handler Html
getSemestersR schule = do
  defaultLayout $ do
    $(widgetFile "semesters")
