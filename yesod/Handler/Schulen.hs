module Handler.Schulen where

import Import
import Control.Schule.DB as SchuleDB
import Control.Schule.Typ as Schule
import Control.Types

getSchulenR :: Handler Html
getSchulenR = do
  schulen <- lift $ SchuleDB.get
  mid <- maybeAuthId
  schulenAutorisiert' <- lift $ mapM (autorisiertSchule mid) schulen
  let schulenAutorisiert = concat schulenAutorisiert'
  defaultLayout $ do
    $(widgetFile "schulen")

autorisiertSchule :: Maybe (AuthId Autotool) -> Schule -> IO [(Text, Maybe (Route Autotool), Maybe (Route Autotool))]
autorisiertSchule mid schule = do
  let UNr u = Schule.unr schule
      Name sname = Schule.name schule
      semesterRoute = SemestersR u
      bearbeitenRoute = SchuleR u
      ist (Just True) route = Just route
      ist _ _ = Nothing
  autorisiertS <- istAutorisiert mid semesterRoute
  autorisiertB <- istAutorisiert mid bearbeitenRoute
  if autorisiertS == Just True || autorisiertB == Just True
     then return [(pack sname
                  ,ist autorisiertS semesterRoute
                  ,ist autorisiertB bearbeitenRoute)] 
     else return []
