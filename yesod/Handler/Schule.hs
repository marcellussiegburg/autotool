module Handler.Schule where

import Import
import qualified Control.Schule.DB as SchuleDB
import Control.Schule.Typ as Schule
import Control.Types
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
postSchuleR schule = do
  mschule <- lift $ liftM listToMaybe $ SchuleDB.get_unr $ UNr schule
  ((result, formWidget), formEnctype) <- runFormPost $ schuleForm mschule
  case result of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess schule' -> do
      _ <- lift $ SchuleDB.put (Just $ unr schule') schule'
      setMessageI MsgSchuleBearbeitet
  defaultLayout $ do
    $(widgetFile "schule")

schuleForm :: Maybe Schule -> Form Schule
schuleForm mschule = do
  let fromName = pack . toString
      toName = Name . unpack
  renderBootstrap3 BootstrapBasicForm $ Schule
    <$> pure (maybe undefined unr mschule)
    <*> (toName <$> areq textField (bfs MsgSchuleName) (fromName . name <$> mschule))
    <*> (toName . maybe "" id <$> aopt textField (bfs MsgSchuleSuffix) (Just . fromName . mail_suffix <$> mschule))
    <*> pure (maybe False use_shibboleth mschule)
    <*> areq (selectField spracheOptionen) (bfs MsgSchuleSprache) (preferred_language <$> mschule)
    <* bootstrapSubmit (BootstrapSubmit (maybe MsgSchuleAnlegen (\ _ -> MsgSchuleBearbeiten) mschule) "btn-success" [])
