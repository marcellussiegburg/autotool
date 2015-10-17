module Handler.TutorErnennen where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import qualified Control.Tutor.DB as TutorDB
import Control.Types

getTutorErnennenR :: VorlesungId -> Handler Html
getTutorErnennenR = postTutorErnennenR

postTutorErnennenR :: VorlesungId -> Handler Html
postTutorErnennenR vorlesungId = do
  vorlesung <- runDB $ get404 vorlesungId
  studenten' <- liftIO $ (StudentDB.get_unr . UNr . keyToInt . vorlesungSchuleId) vorlesung
  let vorlesung' = entityToVorlesung vorlesungId vorlesung : []
      keineTutoren = do
        tutoren <- liftM concat $ mapM TutorDB.get_tutors vorlesung'
        return $ deleteFirstsBy ((==) `on` Student.snr) studenten' tutoren
  let studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineStudentenErnennen,
        submit = BootstrapSubmit MsgTutorErnennen "btn-success btn-block" [],
        erfolgMsg = MsgTutorErnannt,
        formRoute = TutorErnennenR vorlesungId,
        getOp = keineTutoren,
        setOp = \stud -> sequence_ $ map (TutorDB.put stud) vorlesung'
      }
  rolleSetzenListe studentenSeite
