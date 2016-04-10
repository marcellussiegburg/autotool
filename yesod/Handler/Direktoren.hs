module Handler.Direktoren where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)

getDirektorenR :: SchuleId -> Handler Html
getDirektorenR = postDirektorenR

postDirektorenR :: SchuleId -> Handler Html
postDirektorenR schuleId = do
  _ <- runDB $ get404 schuleId
  let direktoren = do
        direktoren' <- selectList [DirektorSchuleId ==. schuleId] []
        concat <$> mapM (\d -> selectList [StudentId ==. direktorStudentId (entityVal d)] []) direktoren'
      studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineDirektoren,
        submit = BootstrapSubmit MsgDirektorAbsetzen "btn-danger btn-block" [],
        erfolgMsg = MsgDirektorAbgesetzt,
        formRoute = DirektorenR schuleId,
        getOp = runDB direktoren,
        setOp = \stud -> runDB $ delete $ DirektorKey (entityKey stud) schuleId
      }
  rolleSetzenListe studentenSeite
