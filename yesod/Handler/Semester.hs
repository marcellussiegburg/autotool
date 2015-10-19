module Handler.Semester where

import Import

getSemesterR :: SemesterId -> Handler Html
getSemesterR = postSemesterR

postSemesterR :: SemesterId -> Handler Html
postSemesterR semesterId = do
  semester <- runDB $ get404 semesterId
  ((result, formWidget), formEnctype) <- runFormPost $ semesterForm (semesterSchuleId semester) $ Just semester
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess semester' -> do
      runDB $ replace semesterId semester'
      setMessageI MsgSemesterBearbeitet
  defaultLayout $
    formToWidget (SemesterR semesterId) Nothing formEnctype formWidget

semesterForm :: SchuleId -> Maybe Semester -> Form Semester
semesterForm schuleId msemester = do
  renderBootstrap3 BootstrapBasicForm $ Semester
    <$> pure schuleId
    <*> areq textField (bfs MsgSemesterName) (semesterName <$> msemester)
    <*> (UTCTime
         <$> areq (jqueryDayField def) (bfsFormControl MsgSemesterBeginnDatum) (utctDay . semesterVon <$> msemester)
         <*> (timeOfDayToTime <$> areq timeFieldTypeTime (bfs MsgSemesterBeginnZeit) (timeToTimeOfDay . utctDayTime . semesterVon <$> msemester)))
    <*> (UTCTime
         <$> areq (jqueryDayField def) (bfsFormControl MsgSemesterEndeDatum) (utctDay . semesterBis <$> msemester)
         <*> (timeOfDayToTime <$> areq timeFieldTypeTime (bfs MsgSemesterEndeZeit) (timeToTimeOfDay . utctDayTime . semesterBis <$> msemester)))
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgSemesterAnlegen (\ _ -> MsgSemesterBearbeiten) msemester) "btn-success" [])
