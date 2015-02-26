module Handler.Schulen where

import Import

getSchulenR :: Handler Html
getSchulenR = do
  schulen <- runDB $ selectList [] []
  mid <- maybeAuthId
  schulenAutorisiert' <- mapM (autorisiertSchule mid) schulen
  let schulenAutorisiert = concat schulenAutorisiert'
  defaultLayout $ do
    $(widgetFile "schulen")

autorisiertSchule :: Maybe (AuthId Autotool) -> Entity Schule -> Handler [(Text, Maybe (Route Autotool), Maybe (Route Autotool))]
autorisiertSchule mid schule = do
  let schuleRoute = SemestersR $ entityKey schule
      bearbeitenRoute = SchuleR $ entityKey schule
      ist (Just True) route = Just route
      ist _ _ = Nothing
  autorisiertS <- istAutorisiert mid schuleRoute
  autorisiertB <- istAutorisiert mid bearbeitenRoute
  if autorisiertS == Just True || autorisiertB == Just True
     then return [(schuleName $ entityVal schule
                  ,ist autorisiertS schuleRoute
                  ,ist autorisiertB bearbeitenRoute)]
     else return []
