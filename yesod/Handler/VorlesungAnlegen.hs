module Handler.VorlesungAnlegen where

import Import
import Handler.Vorlesung (vorlesungForm)

import qualified Control.Vorlesung.DB as VorlesungDB
import Control.Vorlesung.Typ
import Control.Types

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
      _ <- lift $ VorlesungDB.put Nothing vorlesung' { unr = UNr $ keyToInt $ semesterSchuleId semester, enr = ENr $ keyToInt semesterId }
      _ <- setMessageI MsgVorlesungAngelegt
      redirect $ VorlesungenR semesterId -- TODO: VorlesungR verwenden (zu neu erstellter Vorlesung gehen)
  defaultLayout $
    [whamlet|^{formToWidget (VorlesungAnlegenR semesterId) Nothing formEnctype formWidget}|]
