module Handler.TutorErnennen where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import Control.Types

getTutorErnennenR :: VorlesungId -> Handler Html
getTutorErnennenR = postTutorErnennenR

postTutorErnennenR :: VorlesungId -> Handler Html
postTutorErnennenR vorlesungId = do
  vorlesung <- runDB $ get404 vorlesungId
  studenten' <- liftIO $ (StudentDB.get_unr . UNr . keyToInt . vorlesungSchuleId) vorlesung
  let keineTutoren = do
        tutoren' <- runDB $ selectList [TutorVorlesungId ==. vorlesungId] []
        tutoren <- liftIO $ fmap concat $ mapM (StudentDB.get_snr . SNr . tutorStudentId . entityVal) tutoren'
        return $ deleteFirstsBy ((==) `on` Student.snr) studenten' tutoren
  let studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineStudentenErnennen,
        submit = BootstrapSubmit MsgTutorErnennen "btn-success btn-block" [],
        erfolgMsg = MsgTutorErnannt,
        formRoute = TutorErnennenR vorlesungId,
        getOp = keineTutoren,
        setOp = \stud ->
            let SNr studId = Student.snr stud
            in runDB $ deleteBy $ UniqueTutor studId vorlesungId
      }
  rolleSetzenListe studentenSeite
