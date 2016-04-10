module Handler.TutorErnennen where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)

getTutorErnennenR :: VorlesungId -> Handler Html
getTutorErnennenR = postTutorErnennenR

postTutorErnennenR :: VorlesungId -> Handler Html
postTutorErnennenR vorlesungId = do
  vorlesung <- runDB $ get404 vorlesungId
  studenten' <- runDB $ selectList [StudentSchuleId ==. vorlesungSchuleId vorlesung] []
  let keineTutoren = do
        tutoren' <- selectList [TutorVorlesungId ==. vorlesungId] []
        tutoren <- concat <$> mapM (\t -> selectList [StudentId ==. tutorStudentId (entityVal t)] []) tutoren'
        return $ deleteFirstsBy ((==) `on` entityKey) studenten' tutoren
  let studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineStudentenErnennen,
        submit = BootstrapSubmit MsgTutorErnennen "btn-success btn-block" [],
        erfolgMsg = MsgTutorErnannt,
        formRoute = TutorErnennenR vorlesungId,
        getOp = runDB keineTutoren,
        setOp = \stud -> runDB $ delete $ TutorKey (entityKey stud) vorlesungId
      }
  rolleSetzenListe studentenSeite
