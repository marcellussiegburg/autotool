module Handler.Server where

import Import
import Service.Interface (get_task_types)

getServerR :: ServerUrl -> Handler Html
getServerR server = do
  aufgabenTypen <- lift $ liftM (map taskTreeToTextTree) $ get_task_types $ unpack server
  defaultLayout $ do
    addStylesheet $ StaticR css_tree_css
    $(widgetFile "server")

unterbaum :: ServerUrl -> Tree Text -> Maybe Text -> Widget
unterbaum server baum mName = do
  inputId <- newIdent
  $(widgetFile "baum")
