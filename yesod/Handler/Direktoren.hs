module Handler.Direktoren where

import Import
import Handler.DirektorErnennen (StudentenSeite (..), rolleSetzenListe)
import qualified Control.Direktor.DB as DirektorDB
import qualified Control.Schule.DB as SchuleDB
import Control.Types

getDirektorenR :: SchuleId -> Handler Html
getDirektorenR = postDirektorenR

postDirektorenR :: SchuleId -> Handler Html
postDirektorenR schule = do
  schule' <- lift $ SchuleDB.get_unr $ UNr schule
  let direktoren = liftM concat $ mapM DirektorDB.get_directors schule'
      studentenSeite = StudentenSeite {
        titel = MsgDirektoren,
        nullStudenten = MsgKeineDirektoren,
        submit = BootstrapSubmit MsgDirektorAbsetzen "btn-danger btn-block" [],
        erfolgMsg = MsgDirektorAbgesetzt,
        formRoute = DirektorenR schule,
        getOp = direktoren,
        setOp = \stud -> sequence_ $ map (DirektorDB.delete stud) schule'
      }
  rolleSetzenListe studentenSeite
