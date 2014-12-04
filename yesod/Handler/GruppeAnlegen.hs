module Handler.GruppeAnlegen where

import Import

data GruppeForm = GruppeForm Text Text Int

getGruppeAnlegenR :: VorlesungId -> Handler Html
getGruppeAnlegenR vorlesung = do
  (formWidget, formEnctype) <- generateFormPost gruppeForm
  defaultLayout $ do
    $(widgetFile "gruppeAnlegen")

postGruppeAnlegenR :: VorlesungId -> Handler Html
postGruppeAnlegenR vorlesung = do
  ((result, formWidget), formEnctype) <- runFormPost gruppeForm
  defaultLayout $ do
    $(widgetFile "gruppeAnlegen")

gruppeForm :: Form GruppeForm
gruppeForm = do
  renderBootstrap3 BootstrapBasicForm $ GruppeForm
    <$> areq textField (bfs MsgGruppeName) Nothing
    <*> areq textField (bfs MsgReferent) Nothing
    <*> areq intField (bfs MsgPlätze) Nothing
    <* bootstrapSubmit (BootstrapSubmit MsgGruppeAnlegen "btn-success" [])
