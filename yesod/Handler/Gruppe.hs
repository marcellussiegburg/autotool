{-# LANGUAGE OverloadedStrings #-}
module Handler.Gruppe where

import Import

data GruppeForm = GruppeForm {
    name :: Text,
    referent :: Text,
    plätze :: Int
}

getGruppeR :: GruppeId -> Handler Html
getGruppeR gruppe = do
  let gruppe' = Just $ GruppeForm "14IN-B" "Herr Müller" 30
  (formWidget, formEnctype) <- generateFormPost $ gruppeForm gruppe'
  defaultLayout $ do
    $(widgetFile "gruppe")

postGruppeR :: GruppeId -> Handler Html
postGruppeR gruppe = do
  let gruppe' = Just $ GruppeForm "14IN-B" "Herr Müller" 30
  ((result, formWidget), formEnctype) <- runFormPost $ gruppeForm gruppe'
  defaultLayout $ do
    $(widgetFile "gruppe")

gruppeForm :: Maybe GruppeForm -> Form GruppeForm
gruppeForm mgruppe = do
  renderBootstrap3 BootstrapBasicForm $ GruppeForm
    <$> areq textField (bfs MsgGruppeName) (fmap name mgruppe)
    <*> areq textField (bfs MsgReferent) (fmap referent mgruppe)
    <*> areq intField (bfs MsgPlätze) (fmap plätze mgruppe)
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgGruppeAnlegen (\ _ -> MsgGruppeBearbeiten) mgruppe) "btn-success" [])
    <* maybe (pure ()) (\_ -> bootstrapSubmit (BootstrapSubmit MsgLöschen "btn-danger" [])) mgruppe
