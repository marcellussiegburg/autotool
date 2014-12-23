{-# LANGUAGE OverloadedStrings #-}
module Handler.Servers where

import Import

getServersR :: Handler Html
getServersR = do
  (formWidget, formEnctype) <- generateFormPost serversForm
  defaultLayout $ do
    $(widgetFile "servers")

postServersR :: Handler Html
postServersR = do
  ((result, formWidget), formEnctype) <- runFormPost serversForm
  defaultLayout $ do
    $(widgetFile "servers")

serversForm :: Form ServerUrl
serversForm = do
  identifyForm "server" $ renderBootstrap3 BootstrapBasicForm $
    areq textField (withAutofocus $ bfs MsgServer) Nothing
    <* bootstrapSubmit (BootstrapSubmit MsgServerWählen "btn-success" [])
