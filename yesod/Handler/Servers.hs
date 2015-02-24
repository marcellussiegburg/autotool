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
  ((result, formWidget), formEnctype) <- runFormPost $ serversForm Nothing
  case result of
    FormSuccess s -> redirect $ ServerR s
    _ -> return ()
  defaultLayout $
    formToWidget ServersR Nothing formEnctype formWidget

serversForm :: Maybe ServerUrl -> Form ServerUrl
serversForm mserver = do
  renderBootstrap3 BootstrapBasicForm $
    areq serverField (withAutofocus $ bfs MsgServer) (Just $ maybe (pack server) id mserver)
    <* bootstrapSubmit (BootstrapSubmit MsgServerWählen "btn-success" [])
  where
    serverField = flip checkM textField $ \ server' -> do
      check' <- lift $ try $ get_task_types $ unpack server' :: HandlerT Autotool IO (Either IOException [TaskTree])
      case check' of
        Left e -> return . Left . pack . show $ e
        Right _ -> return $ Right server'
