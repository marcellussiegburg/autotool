{-# LANGUAGE OverloadedStrings #-}
module Handler.Semester where

import Import

data SemesterForm = SemesterForm {
    name :: Text,
    beginn :: Day,
    beginnZeit :: TimeOfDay,
    ende :: Day,
    endeZeit :: TimeOfDay
}

getSemesterR :: SemesterId -> Handler Html
getSemesterR semester = do
  let beginn' = fromGregorian 2014 10 1
      beginnZeit' = midnight
      ende' = fromGregorian 2015 3 31
      endeZeit' = midnight
      semester' = Just $ SemesterForm "WS 2014/2015" beginn' beginnZeit' ende' endeZeit'
  (formWidget, formEnctype) <- generateFormPost $ semesterForm semester'
  defaultLayout $ do
    $(widgetFile "semester")

postSemesterR :: SemesterId -> Handler Html
postSemesterR semester = do
  let beginn' = fromGregorian 2014 10 1
      beginnZeit' = midnight
      ende' = fromGregorian 2015 3 31
      endeZeit' = midnight
      semester' = Just $ SemesterForm "WS 2014/2015" beginn' beginnZeit' ende' endeZeit'
  ((result, formWidget), formEnctype) <- runFormPost $ semesterForm semester'
  defaultLayout $ do
    $(widgetFile "semester")

semesterForm :: Maybe SemesterForm -> Form SemesterForm
semesterForm msemester = do
  renderBootstrap3 BootstrapBasicForm $ SemesterForm
    <$> areq textField (bfs MsgSemesterName) (fmap name msemester)
    <*> areq (jqueryDayField def) (bfsFormControl MsgSemesterBeginnDatum) (fmap beginn msemester)
    <*> areq timeField (bfs MsgSemesterBeginnZeit) (fmap beginnZeit msemester)
    <*> areq (jqueryDayField def) (bfsFormControl MsgSemesterEndeDatum) (fmap ende msemester)
    <*> areq timeField (bfs MsgSemesterEndeZeit) (fmap endeZeit msemester)
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgSemesterAnlegen (\ _ -> MsgSemesterBearbeiten) msemester) "btn-success" [])
