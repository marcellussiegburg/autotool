module Handler.Studenten where

import Import

getStudentenR :: VorlesungId -> Handler Html
getStudentenR vorlesung = do
  defaultLayout $ do
    $(widgetFile "studenten")
