module Handler.AufgabeBenutzerId where

import Import

getAufgabeBenutzerIdR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeBenutzerIdR = postAufgabeBenutzerIdR

postAufgabeBenutzerIdR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeBenutzerIdR server aufgabeTyp konfiguration = do
  ((result, formWidget), formEnctype) <- runFormPost benutzerIdForm
  case result of
    FormSuccess bid -> redirect $ AufgabeTestenR server aufgabeTyp konfiguration $ pack $ show bid
    _ -> defaultLayout $
           $(widgetFile "aufgabeBenutzerId")

benutzerIdForm :: Form Integer
benutzerIdForm = renderBootstrap3 BootstrapBasicForm $
  areq intField (bfs MsgAufgabeBenutzerId) Nothing
  <* bootstrapSubmit (BootstrapSubmit MsgAufgabeBenutzerId "btn-success" [])
