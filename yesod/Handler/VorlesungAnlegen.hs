module Handler.VorlesungAnlegen where

import Import
import Handler.Vorlesung (vorlesungForm)

getVorlesungAnlegenR :: SemesterId -> Handler Html
getVorlesungAnlegenR = postVorlesungAnlegenR

postVorlesungAnlegenR :: SemesterId -> Handler Html
postVorlesungAnlegenR semesterId = do
  Just semester <- runDB $ get semesterId
  ((result, formWidget), formEnctype) <- runFormPost $ vorlesungForm (semesterSchuleId semester) semesterId Nothing
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess vorlesung' -> do
      vorlesungId <- runDB $ insert vorlesung'
      _ <- setMessageI MsgVorlesungAngelegt
      redirect $ VorlesungR vorlesungId
  defaultLayout $
    [whamlet|^{formToWidget (VorlesungAnlegenR semesterId) Nothing formEnctype formWidget}|]
