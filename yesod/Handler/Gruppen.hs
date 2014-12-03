module Handler.Gruppen where

import Import
import Prelude (undefined)

getGruppenR :: VorlesungId -> Handler Html
getGruppenR vorlesung = do
  defaultLayout $ do
    $(widgetFile "gruppen")

postGruppenR :: VorlesungId -> Handler Html
postGruppenR vorlesung = undefined
