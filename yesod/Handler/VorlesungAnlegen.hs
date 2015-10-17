module Handler.VorlesungAnlegen where

import Import
import Handler.Vorlesung (vorlesungForm)

getVorlesungAnlegenR :: SemesterId -> Handler Html
getVorlesungAnlegenR = postVorlesungAnlegenR

postVorlesungAnlegenR :: SemesterId -> Handler Html
postVorlesungAnlegenR semesterId = do
  ((result, formWidget), formEnctype) <- runFormPost $ vorlesungForm Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess vorlesung' -> do
      Just semester <- runDB $ get semesterId
      vorlesungId <- runDB $ insert vorlesung' { vorlesungSchuleId = semesterSchuleId semester, vorlesungSemesterId = semesterId }
      _ <- setMessageI MsgVorlesungAngelegt
      redirect $ VorlesungR vorlesungId
  defaultLayout $
    [whamlet|^{formToWidget (VorlesungAnlegenR semesterId) Nothing formEnctype formWidget}|]
