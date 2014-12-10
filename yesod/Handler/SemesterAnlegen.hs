module Handler.SemesterAnlegen where

import Import
import Handler.Semester (semesterForm)

getSemesterAnlegenR :: SchuleId -> Handler Html
getSemesterAnlegenR schule = do
  (formWidget, formEnctype) <- generateFormPost $ semesterForm Nothing
  defaultLayout $ do
    $(widgetFile "semesterAnlegen")

postSemesterAnlegenR :: SchuleId -> Handler Html
postSemesterAnlegenR schule = do
  ((result, formWidget), formEnctype) <- runFormPost $ semesterForm Nothing
  defaultLayout $ do
    $(widgetFile "semesterAnlegen")
