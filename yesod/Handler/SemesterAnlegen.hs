module Handler.SemesterAnlegen where

import Import
import Handler.Semester (semesterForm)

import qualified Control.Schule.DB as SchuleDB
import qualified Control.Schule.Typ as Schule
import qualified Control.Semester.DB as SemesterDB
import Control.Semester.Typ
import Control.Types

getSemesterAnlegenR :: SchuleId -> Handler Html
getSemesterAnlegenR = postSemesterAnlegenR

postSemesterAnlegenR :: SchuleId -> Handler Html
postSemesterAnlegenR schule = do
  Just schule' <- lift $ liftM listToMaybe $ SchuleDB.get_unr $ UNr schule
  ((result, formWidget), formEnctype) <- runFormPost $ semesterForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess semester' -> do
      _ <- lift $ SemesterDB.put Nothing semester' { unr = Schule.unr schule' }
      _ <- setMessageI MsgSemesterAngelegt
      let UNr s = Schule.unr schule'
      redirect $ SemestersR s -- ^ TODO: SemesterR verwenden (zu neu erstelltem Semester gehen)
  defaultLayout $ do
    $(widgetFile "semesterAnlegen")
