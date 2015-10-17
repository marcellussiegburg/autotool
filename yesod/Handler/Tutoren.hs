module Handler.Tutoren where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)
import qualified Control.Tutor.DB as TutorDB

getTutorenR :: VorlesungId -> Handler Html
getTutorenR = postTutorenR

postTutorenR :: VorlesungId -> Handler Html
postTutorenR vorlesungId = do
  mvorlesung <- runDB $ get vorlesungId
  let mvorlesung' = maybeToList $ fmap (entityToVorlesung vorlesungId) mvorlesung
  let tutoren = liftM concat $ mapM TutorDB.get_tutors $ mvorlesung'
      studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineTutoren,
        submit = BootstrapSubmit MsgTutorAbsetzen "btn-danger btn-block" [],
        erfolgMsg = MsgTutorAbgesetzt,
        formRoute = TutorenR vorlesungId,
        getOp = tutoren,
        setOp = \stud -> sequence_ $ fmap (TutorDB.delete stud) mvorlesung'
      }
  rolleSetzenListe studentenSeite
