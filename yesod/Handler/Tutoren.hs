module Handler.Tutoren where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)
import qualified Control.Tutor.DB as TutorDB
import qualified Control.Vorlesung.DB as VorlesungDB
import Control.Types

getTutorenR :: VorlesungId -> Handler Html
getTutorenR = postTutorenR

postTutorenR :: VorlesungId -> Handler Html
postTutorenR vorlesung = do
  vorlesung' <- lift $ VorlesungDB.get_this $ VNr vorlesung
  let tutoren = liftM concat $ mapM TutorDB.get_tutors vorlesung'
      studentenSeite = StudentenSeite {
        titel = MsgTutoren,
        nullStudenten = MsgKeineTutoren,
        submit = BootstrapSubmit MsgTutorAbsetzen "btn-danger btn-block" [],
        erfolgMsg = MsgTutorAbgesetzt,
        formRoute = TutorenR vorlesung,
        getOp = tutoren,
        setOp = \stud -> sequence_ $ map (TutorDB.delete stud) vorlesung'
      }
  rolleSetzenListe studentenSeite
