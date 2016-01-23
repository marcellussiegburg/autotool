module Handler.AufgabeVorlagen where

import Import

getAufgabeVorlagenR :: ServerUrl -> AufgabeTyp -> Handler Html
getAufgabeVorlagenR server typ = do
  vorlagen <- getVorlagen typ
  defaultLayout $
    $(widgetFile "aufgabeVorlagen")

getVorlagen :: AufgabeTyp -> Handler [Text]
getVorlagen typ = do
  aufgaben <- runDB $ selectList [AufgabeTyp ==. typ] []
  return $ map (aufgabeName . entityVal) aufgaben
