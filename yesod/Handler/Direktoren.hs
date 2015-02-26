module Handler.Direktoren where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)
import qualified Control.Direktor.DB as DirektorDB
import qualified Control.Schule.DB as SchuleDB
import Control.Types

getDirektorenR :: SchuleId -> Handler Html
getDirektorenR = postDirektorenR

postDirektorenR :: SchuleId -> Handler Html
postDirektorenR schuleId = do
  schule <- runDB $ get404 schuleId
  let schule' = entityToSchule schuleId schule
      direktoren = DirektorDB.get_directors schule'
      studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineDirektoren,
        submit = BootstrapSubmit MsgDirektorAbsetzen "btn-danger btn-block" [],
        erfolgMsg = MsgDirektorAbgesetzt,
        formRoute = DirektorenR schuleId,
        getOp = direktoren,
        setOp = \stud -> DirektorDB.delete stud schule'
      }
  rolleSetzenListe studentenSeite
