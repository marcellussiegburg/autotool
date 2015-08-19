module Handler.SemesterAnlegen where

import Import
import Handler.Semester (semesterForm)

import qualified Control.Semester.DB as SemesterDB
import Control.Semester.Typ
import Control.Types

getSemesterAnlegenR :: SchuleId -> Handler Html
getSemesterAnlegenR = postSemesterAnlegenR

postSemesterAnlegenR :: SchuleId -> Handler Html
postSemesterAnlegenR schule = do
  _ <- runDB $ get404 schule
  ((result, formWidget), formEnctype) <- runFormPost $ semesterForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess semester' -> do
      _ <- lift $ SemesterDB.put Nothing semester' { unr = UNr $ keyToInt $ schule }
      _ <- setMessageI MsgSemesterAngelegt
      redirect $ SemestersR schule -- TODO: SemesterR verwenden (zu neu erstelltem Semester gehen)
  defaultLayout $
    formToWidget (SemesterAnlegenR schule) Nothing formEnctype formWidget
