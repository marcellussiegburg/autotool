module Handler.SchuleAnlegen where

import Import
import Handler.Schule (schuleForm)
import Control.Schule.DB as SchuleDB

getSchuleAnlegenR :: Handler Html
getSchuleAnlegenR = postSchuleAnlegenR

postSchuleAnlegenR :: Handler Html
postSchuleAnlegenR = do
  ((result, formWidget), formEnctype) <- runFormPost $ schuleForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess schule' -> do
      _ <- lift $ SchuleDB.put Nothing schule'
      _ <- setMessageI MsgSchuleAngelegt
      redirect SchulenR -- ^ TODO: SchuleR verwenden (zu neu erstellter Schule gehen)
  defaultLayout $
    formToWidget SchuleAnlegenR Nothing formEnctype formWidget
