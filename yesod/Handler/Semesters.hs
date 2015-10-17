module Handler.Semesters where

import Import

import Control.Types
import Data.Time (getCurrentTime)
import Autolib.Util.Sort (sortBy)

getSemestersR :: SchuleId -> Handler Html
getSemestersR schuleId = do
  semesters <- runDB $ selectList [SemesterSchuleId ==. schuleId] []
  zeit <- liftIO $ getCurrentTime
  let semestersSortiert = sortBy (\ s -> zeitStatus (semesterVon $ entityVal s) (semesterBis $ entityVal s) zeit /= Current) semesters
  mid <- maybeAuthId
  semestersAutorisiert' <- mapM (autorisiertSemester mid) semestersSortiert
  let semestersAutorisiert = concat semestersAutorisiert'
  defaultLayout $ do
    $(widgetFile "semesters")

autorisiertSemester :: Maybe (AuthId Autotool) -> Entity Semester -> Handler [(Semester, Maybe (Route Autotool), Maybe (Route Autotool))]
autorisiertSemester mid semester = do
  let semesterRoute = VorlesungenR $ entityKey semester
      bearbeitenRoute = SemesterR $ entityKey semester
      ist (Just True) route = Just route
      ist _ _ = Nothing
  autorisiertS <- istAutorisiert mid semesterRoute
  autorisiertB <- istAutorisiert mid bearbeitenRoute
  if autorisiertS == Just True || autorisiertB == Just True
     then return [(entityVal semester
                  ,ist autorisiertS semesterRoute
                  ,ist autorisiertB bearbeitenRoute)]
     else return []

listGroupItemClass :: TimeStatus -> Text
listGroupItemClass status = case status of
  Early -> "list-group-item-warning"
  Late -> "list-group-item-danger"
  Current -> "list-group-item-success"
