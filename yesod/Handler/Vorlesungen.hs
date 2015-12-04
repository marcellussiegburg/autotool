module Handler.Vorlesungen where

import Import

import Control.Types (TimeStatus (Early, Late, Current))
import Data.Time (getCurrentTime)

getVorlesungenR :: SemesterId -> Handler Html
getVorlesungenR semester = do
  vorlesungen <- runDB $ selectList [VorlesungSemesterId ==. semester] [Desc VorlesungVon]
  mid <- maybeAuthId
  vorlesungenAutorisiert' <- mapM (autorisiertVorlesung mid) vorlesungen
  let vorlesungenAutorisiert = concat vorlesungenAutorisiert'
  zeit <- liftIO getCurrentTime
  defaultLayout $
    $(widgetFile "vorlesungen")

autorisiertVorlesung :: Maybe (AuthId Autotool) -> Entity Vorlesung -> Handler [(Vorlesung, Maybe (Route Autotool), Maybe (Route Autotool))]
autorisiertVorlesung mid vorlesung = do
  let vorlesungRoute = GruppenR $ entityKey vorlesung
      bearbeitenRoute = VorlesungR $ entityKey vorlesung
      ist (Just True) route = Just route
      ist _ _ = Nothing
  autorisiertS <- istAutorisiert mid vorlesungRoute
  autorisiertB <- istAutorisiert mid bearbeitenRoute
  if autorisiertS == Just True || autorisiertB == Just True
     then return [(entityVal vorlesung
                  ,ist autorisiertS vorlesungRoute
                  ,ist autorisiertB bearbeitenRoute)]
     else return []

statusClass :: TimeStatus -> Text
statusClass status = case status of
  Early -> "warning"
  Late -> "danger"
  Current -> "success"
