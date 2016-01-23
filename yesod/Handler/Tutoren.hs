module Handler.Tutoren where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)

import qualified Control.Student.DB as StudentDB
import Control.Types
import Control.Student.Type

getTutorenR :: VorlesungId -> Handler Html
getTutorenR = postTutorenR

postTutorenR :: VorlesungId -> Handler Html
postTutorenR vorlesungId = do
  vorlesung <- runDB $ get404 vorlesungId
  let tutoren = do
        tutoren' <- runDB $ selectList [TutorVorlesungId ==. vorlesungId] []
        liftIO $ fmap concat $ mapM (StudentDB.get_snr . SNr . tutorStudentId . entityVal) tutoren' 
      studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineTutoren,
        submit = BootstrapSubmit MsgTutorAbsetzen "btn-danger btn-block" [],
        erfolgMsg = MsgTutorAbgesetzt,
        formRoute = TutorenR vorlesungId,
        getOp = tutoren,
        setOp = \stud ->
            let SNr studId = snr stud
            in runDB $ deleteBy $ UniqueTutor studId vorlesungId
      }
  rolleSetzenListe studentenSeite
