module Handler.SchuleAnlegen where

import Import
import Handler.Schule (schuleForm)

getSchuleAnlegenR :: Handler Html
getSchuleAnlegenR = postSchuleAnlegenR

postSchuleAnlegenR :: Handler Html
postSchuleAnlegenR = do
  ((result, formWidget), formEnctype) <- runFormPost $ schuleForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess schule' -> do
      schuleId <- runDB $ insert schule'
      _ <- setMessageI MsgSchuleAngelegt
      redirect $ SchuleR schuleId
  defaultLayout $
    formToWidget SchuleAnlegenR Nothing formEnctype formWidget
