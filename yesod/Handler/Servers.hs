module Handler.Servers where

import Import
import Control.Exception (IOException, try)

import Default (server)
import Service.Interface (get_task_types)
import Types.TaskTree (TaskTree)

getServersR :: Handler Html
getServersR = postServersR

postServersR :: Handler Html
postServersR = do
  ((result, formWidget), formEnctype) <- runFormPost serversForm
  case result of
    FormSuccess s -> redirect $ ServerR s
    _ -> return ()
  defaultLayout $ do
    $(widgetFile "servers")

serversForm :: Form ServerUrl
serversForm = do
  identifyForm "server" $ renderBootstrap3 BootstrapBasicForm $
    areq serverField (withAutofocus $ bfs MsgServer) (Just $ pack server)
    <* bootstrapSubmit (BootstrapSubmit MsgServerWählen "btn-success" [])
  where
    serverField = flip checkM textField $ \ server' -> do
      check' <- lift $ try $ get_task_types $ unpack server' :: HandlerT Autotool IO (Either IOException [TaskTree])
      case check' of
        Left e -> return . Left . pack . show $ e
        Right _ -> return $ Right server'
