{-# LANGUAGE OverloadedStrings #-}
module Handler.Schule where

import Import

data SchuleForm = SchuleForm {
    name :: Text,
    suffix :: Maybe Text
}

getSchuleR :: SchuleId -> Handler Html
getSchuleR schule = do
  let schule' = Just $ SchuleForm "HTWK Leipzig" $ Just "htwk-leipzig.de"
  (formWidget, formEnctype) <- generateFormPost $ schuleForm schule'
  defaultLayout $ do
    $(widgetFile "schule")

postSchuleR :: SchuleId -> Handler Html
postSchuleR schule = do
  let schule' = Just $ SchuleForm "HTWK Leipzig" $ Just "htwk-leipzig.de"
  ((result, formWidget), formEnctype) <- runFormPost $ schuleForm schule'
  defaultLayout $ do
    $(widgetFile "schule")

schuleForm :: Maybe SchuleForm -> Form SchuleForm
schuleForm mschule = do
  renderBootstrap3 BootstrapBasicForm $ SchuleForm
    <$> areq textField (bfs MsgSchuleName) (fmap name mschule)
    <*> aopt textField (bfs MsgSchuleSuffix) (fmap suffix mschule)
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgSchuleAnlegen (\ _ -> MsgSchuleBearbeiten) mschule) "btn-success" [])
