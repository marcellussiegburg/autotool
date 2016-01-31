module Handler.AufgabeBenutzerId where

import Import

getAufgabeBenutzerIdR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeBenutzerIdR = postAufgabeBenutzerIdR

postAufgabeBenutzerIdR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeBenutzerIdR server aTyp konfiguration = do
  ((result, formWidget), formEnctype) <- runFormPost benutzerIdForm
  case result of
    FormSuccess bid -> redirect $ AufgabeTestenR server aTyp konfiguration $ bid
    _ -> defaultLayout $
           formToWidget (AufgabeBenutzerIdR server aTyp konfiguration) Nothing formEnctype formWidget

benutzerIdForm :: Form Text
benutzerIdForm = renderBootstrap3 BootstrapBasicForm $
  areq textField (bfs MsgAufgabeBenutzerId) Nothing
  <* bootstrapSubmit (BootstrapSubmit MsgAufgabeBenutzerId "btn-success" [])
