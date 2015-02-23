module Handler.TutorErnennen where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import qualified Control.Tutor.DB as TutorDB
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Vorlesung.Typ as Vorlesung
import Control.Types

getTutorErnennenR :: VorlesungId -> Handler Html
getTutorErnennenR = postTutorErnennenR

postTutorErnennenR :: VorlesungId -> Handler Html
postTutorErnennenR vorlesung = do
  vorlesung' <- lift $ VorlesungDB.get_this $ VNr vorlesung
  studenten' <- lift $ liftM concat $ mapM (StudentDB.get_unr . Vorlesung.unr) vorlesung'
  let keineTutoren = do
        tutoren <- liftM concat $ mapM TutorDB.get_tutors vorlesung'
        return $ deleteFirstsBy ((==) `on` Student.snr) studenten' tutoren
  let studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineStudentenErnennen,
        submit = BootstrapSubmit MsgTutorErnennen "btn-success btn-block" [],
        erfolgMsg = MsgTutorErnannt,
        formRoute = TutorErnennenR vorlesung,
        getOp = keineTutoren,
        setOp = \stud -> sequence_ $ map (TutorDB.put stud) vorlesung'
      }
  rolleSetzenListe studentenSeite
