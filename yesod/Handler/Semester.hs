module Handler.Semester where

import Import

import qualified Control.Semester.DB as SemesterDB
import Control.Semester.Typ
import Control.Types

getSemesterR :: SemesterId -> Handler Html
getSemesterR = postSemesterR

postSemesterR :: SemesterId -> Handler Html
postSemesterR semester = do
  msemester <- lift $ liftM listToMaybe $ SemesterDB.get_this $ ENr semester
  ((result, formWidget), formEnctype) <- runFormPost $ semesterForm msemester
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess semester' -> do
      _ <- lift $ SemesterDB.put (Just $ enr semester') semester'
      setMessageI MsgSemesterBearbeitet
  defaultLayout $
    formToWidget (SemesterR semester) Nothing formEnctype formWidget

semesterForm :: Maybe Semester -> Form Semester
semesterForm msemester = do
  let fromName = pack . toString
      toName = Name . unpack
  renderBootstrap3 BootstrapBasicForm $ Semester
    <$> pure (maybe undefined enr msemester)
    <*> pure (maybe undefined unr msemester)
    <*> (toName <$> areq textField (bfs MsgSemesterName) (fromName . name <$> msemester))
    <*> ((\day time -> utcTimeToTime $ UTCTime day time)
         <$> areq (jqueryDayField def) (bfsFormControl MsgSemesterBeginnDatum) (utctDay . timeToUTCTime . von <$> msemester)
         <*> (timeOfDayToTime <$> areq timeField (bfs MsgSemesterBeginnZeit) (timeToTimeOfDay . utctDayTime . timeToUTCTime . von <$> msemester)))
    <*> ((\day time -> utcTimeToTime $ UTCTime day time)
         <$> areq (jqueryDayField def) (bfsFormControl MsgSemesterEndeDatum) (utctDay . timeToUTCTime . bis <$> msemester)
         <*> (timeOfDayToTime <$> areq timeField (bfs MsgSemesterEndeZeit) (timeToTimeOfDay . utctDayTime . timeToUTCTime . von <$> msemester)))
    <*> pure undefined
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgSemesterAnlegen (\ _ -> MsgSemesterBearbeiten) msemester) "btn-success" [])
