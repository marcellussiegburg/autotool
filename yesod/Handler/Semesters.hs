module Handler.Semesters where

import Import

import qualified Control.Semester.DB as SemesterDB
import qualified Control.Semester.Typ as Semester
import Control.Types
import Autolib.Util.Sort (sortBy)

getSemestersR :: SchuleId -> Handler Html
getSemestersR schule = do
  semesters <- lift $ SemesterDB.get_at_school $ UNr schule
  let semestersSortiert = sortBy (\ s -> Semester.status s /= Current) semesters
  mid <- maybeAuthId
  semestersAutorisiert' <- lift $ mapM (autorisiertSemester mid) semestersSortiert
  let semestersAutorisiert = concat semestersAutorisiert'
      sname s = let Name n = Semester.name s
                in n
  defaultLayout $ do
    $(widgetFile "semesters")

autorisiertSemester :: Maybe (AuthId Autotool) -> Semester.Semester -> IO [(Semester.Semester, Maybe (Route Autotool), Maybe (Route Autotool))]
autorisiertSemester mid semester = do
  let ENr s = Semester.enr semester
      semesterRoute = VorlesungenR s
      bearbeitenRoute = SemesterR s
      ist (Just True) route = Just route
      ist _ _ = Nothing
  autorisiertS <- istAutorisiert mid semesterRoute
  autorisiertB <- istAutorisiert mid bearbeitenRoute
  if autorisiertS == Just True || autorisiertB == Just True
     then return [(semester
                  ,ist autorisiertS semesterRoute
                  ,ist autorisiertB bearbeitenRoute)]
     else return []

listGroupItemClass :: TimeStatus -> Text
listGroupItemClass status = case status of
  Early -> "list-group-item-warning"
  Late -> "list-group-item-danger"
  Current -> "list-group-item-success"
