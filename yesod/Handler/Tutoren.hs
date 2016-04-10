module Handler.Tutoren where

import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)

import Import

getTutorenR :: VorlesungId -> Handler Html
getTutorenR = postTutorenR

postTutorenR :: VorlesungId -> Handler Html
postTutorenR vorlesungId = do
  _ <- runDB $ get404 vorlesungId
  let tutoren = do
        tutoren' <- selectList [TutorVorlesungId ==. vorlesungId] []
        tutoren'' <- mapM (\t -> selectList [StudentId ==. tutorStudentId (entityVal t)] []) tutoren' 
        return $ concat tutoren''
      studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineTutoren,
        submit = BootstrapSubmit MsgTutorAbsetzen "btn-danger btn-block" [],
        erfolgMsg = MsgTutorAbgesetzt,
        formRoute = TutorenR vorlesungId,
        getOp = runDB tutoren,
        setOp = runDB . delete . entityKey
      }
  rolleSetzenListe studentenSeite
