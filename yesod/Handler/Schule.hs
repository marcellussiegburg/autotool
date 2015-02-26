module Handler.Schule where

import Import
import Autolib.Multilingual

spracheMsg :: Language -> AutotoolMessage
spracheMsg sprache' = case sprache' of
  DE -> MsgSpracheDE
  NL -> MsgSpracheNL
  UK -> MsgSpracheUK

spracheOptionen :: Handler (OptionList Language)
spracheOptionen = optionsPairs $ map (\s -> (spracheMsg s, s)) [DE, NL, UK]

getSchuleR :: SchuleId -> Handler Html
getSchuleR = postSchuleR

postSchuleR :: SchuleId -> Handler Html
postSchuleR schuleId = do
  schule <- runDB $ get404 schuleId
  ((result, formWidget), formEnctype) <- runFormPost $ schuleForm $ Just schule
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess schule' -> do
      runDB $ replace schuleId schule'
      setMessageI MsgSchuleBearbeitet
  defaultLayout $
    formToWidget (SchuleR schuleId) Nothing formEnctype formWidget

schuleForm :: Maybe Schule -> Form Schule
schuleForm mschule =
  renderBootstrap3 BootstrapBasicForm $ Schule
    <$> areq textField (bfs MsgSchuleName) (schuleName <$> mschule)
    <*> (maybe "" id <$> aopt textField (bfs MsgSchuleSuffix) (Just . schuleMailSuffix <$> mschule))
    <*> pure (maybe False schuleUseShibboleth mschule)
    <*> areq (selectField spracheOptionen) (bfs MsgSchuleSprache) (schulePreferredLanguage <$> mschule)
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgSchuleAnlegen (\ _ -> MsgSchuleBearbeiten) mschule) "btn-success" [])
