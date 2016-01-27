module Handler.Direktoren where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)
import qualified Control.Direktor.DB as DirektorDB
import qualified Control.Student.Type as Student
import Control.Types (SNr (SNr))

getDirektorenR :: SchuleId -> Handler Html
getDirektorenR = postDirektorenR

postDirektorenR :: SchuleId -> Handler Html
postDirektorenR schuleId = do
  schule <- runDB $ get404 schuleId
  let schule' = entityToSchule schuleId schule
      direktoren = DirektorDB.get_directors schule'
      fromSNr (SNr snr) = snr
      studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineDirektoren,
        submit = BootstrapSubmit MsgDirektorAbsetzen "btn-danger btn-block" [],
        erfolgMsg = MsgDirektorAbgesetzt,
        formRoute = DirektorenR schuleId,
        getOp = lift direktoren,
        setOp = \stud -> runDB $ delete $ DirektorKey (fromSNr $ Student.snr stud) schuleId
      }
  rolleSetzenListe studentenSeite
