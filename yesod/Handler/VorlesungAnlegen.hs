module Handler.VorlesungAnlegen where

import Import
import Handler.Vorlesung (vorlesungForm)

import qualified Control.Semester.DB as SemesterDB
import qualified Control.Semester.Typ as Semester
import qualified Control.Vorlesung.DB as VorlesungDB
import Control.Vorlesung.Typ
import Control.Types

getVorlesungAnlegenR :: SemesterId -> Handler Html
getVorlesungAnlegenR = postVorlesungAnlegenR

postVorlesungAnlegenR :: SemesterId -> Handler Html
postVorlesungAnlegenR semester = do
  ((result, formWidget), formEnctype) <- runFormPost $ vorlesungForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess vorlesung' -> do
      Just semester' <- lift $ liftM listToMaybe $ SemesterDB.get_this $ ENr semester
      _ <- lift $ VorlesungDB.put Nothing vorlesung' { unr = Semester.unr semester', enr = Semester.enr semester' }
      _ <- setMessageI MsgVorlesungAngelegt
      let ENr s = Semester.enr semester'
      redirect $ VorlesungenR s -- ^ TODO: VorlesungR verwenden (zu neu erstellter Vorlesung gehen)
  defaultLayout $ do
    $(widgetFile "vorlesungAnlegen")
