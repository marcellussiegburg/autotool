module Handler.SemesterAnlegen where

import Import
import Handler.Semester (semesterForm)

getSemesterAnlegenR :: SchuleId -> Handler Html
getSemesterAnlegenR = postSemesterAnlegenR

postSemesterAnlegenR :: SchuleId -> Handler Html
postSemesterAnlegenR schuleId = do
  _ <- runDB $ get404 schuleId
  ((result, formWidget), formEnctype) <- runFormPost $ semesterForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess semester' -> do
      semester <- runDB $ insert semester' { semesterSchuleId = schuleId }
      _ <- setMessageI MsgSemesterAngelegt
      redirect $ SemesterR semester
  defaultLayout $
    formToWidget (SemesterAnlegenR schuleId) Nothing formEnctype formWidget
