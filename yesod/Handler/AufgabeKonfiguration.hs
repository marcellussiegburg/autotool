module Handler.AufgabeKonfiguration where

import Import
import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html ((!))
import Util.Xml.Output (xmlStringToOutput)
import Yesod.Form.Fields.PreField (preField)

import Autolib.Output (render)
import Autolib.Multilingual (specialize)
import Service.Interface (get_task_description_localized, verify_task_config_localized)
import Types.Basic (Task)
import Types.Config (Config (CString))
import Types.Description (Description (DString))
import Types.Documented (contents, documentation)
import Types.Signed (Signed)
import Types.TaskDescription (task_sample_config)

type Konfiguration = Text

konfigurationId :: Text
konfigurationId = "konfiguration"

getAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeKonfigurationR = postAufgabeKonfigurationR

postAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeKonfigurationR server aufgabeTyp konfiguration = do
  typ <- liftM snd $ getBeispielKonfiguration server aufgabeTyp
  let checkMethode = checkKonfiguration server aufgabeTyp
  ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ konfigurationForm checkMethode typ $ Just konfiguration
  case result of
    FormSuccess k ->
      redirect $ AufgabeKonfigurationR server aufgabeTyp k
    _ -> do
      mhinweis <- getKonfigurationFehler konfiguration checkMethode
      defaultLayout $ do
        $(widgetFile "aufgabeKonfiguration")

getBeispielKonfiguration :: ServerUrl -> AufgabeTyp -> Handler (AufgabeKonfiguration, Html)
getBeispielKonfiguration server aufgabeTyp = do
  sprache <- getBevorzugteSprache
  beispielKonfiguration <- lift $ liftM task_sample_config
    $ get_task_description_localized (unpack server) (unpack aufgabeTyp) sprache
  let DString dokumentation = documentation beispielKonfiguration
      output = xmlStringToOutput dokumentation
      CString konfiguration = contents beispielKonfiguration
  return (pack konfiguration, specialize sprache $ render output)

getKonfigurationFehler :: AufgabeKonfiguration -> (AufgabeKonfiguration -> Handler (Either Description b)) -> Handler (Maybe Html)
getKonfigurationFehler konfiguration checkMethode = runMaybeT $ do
  konfiguration' <- lookupPostParam konfigurationId
  Left (DString d) <- lift $ checkMethode $ maybe konfiguration id konfiguration'
  sprache <- getBevorzugteSprache
  let hinweis = specialize sprache $ render $ xmlStringToOutput d
  return $ hinweis ! class_ "alert-danger"

checkKonfiguration :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler (Either Description (Signed (Task, Config)))
checkKonfiguration server aufgabeTyp konfiguration = do
  sprache <- getBevorzugteSprache
  lift $ verify_task_config_localized (unpack server) (unpack aufgabeTyp) (CString $ unpack $ konfiguration) sprache

konfigurationForm :: ToMarkup t => (AufgabeKonfiguration -> Handler (Either Description b)) -> t -> Maybe AufgabeKonfiguration -> AForm Handler AufgabeKonfiguration
konfigurationForm checkMethode typ mkonfiguration = do
    aopt (preField typ) (bfs MsgAufgabeKonfigurationTyp) {fsAttrs = []} Nothing
    *> areq konfigurationField (addAttrs $ bfs MsgAufgabeKonfiguration) mkonfiguration
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeKonfigurieren "btn-success" [])
  where
    addAttrs field = field {
        fsAttrs = ("rows", pack . show $ 2 + maybe 0 (length . lines) mkonfiguration) : fsAttrs field,
        fsName = Just konfigurationId
      }
    konfigurationField = checkMMap (\ (Textarea konfiguration) -> do
        check' <- checkMethode konfiguration
        case check' of
          Left _ -> return $ Left MsgFehler
          Right _ -> return $ Right konfiguration
      ) Textarea textareaField
