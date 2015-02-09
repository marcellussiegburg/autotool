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
import Types.Documented (documentation)
import Types.Signed (Signed)
import Types.TaskDescription (task_sample_config)

type Konfiguration = Text

konfigurationId :: Text
konfigurationId = "konfiguration"

getAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeKonfigurationR = postAufgabeKonfigurationR

postAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeKonfigurationR server aufgabeTyp konfiguration = do
  sprache <- getBevorzugteSprache
  DString dokumentation <- lift $ liftM (documentation . task_sample_config)
                           $ get_task_description_localized (unpack server) (unpack aufgabeTyp) sprache
  let output = xmlStringToOutput dokumentation
      typ :: Html
      typ = specialize sprache $ render output
      checkMethode = checkKonfiguration server aufgabeTyp
  ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ konfigurationForm checkMethode typ $ Just konfiguration
  konfiguration' <- lookupPostParam konfigurationId
  mhinweis <- case result of
    FormSuccess k -> redirect $ AufgabeKonfigurationR server aufgabeTyp k
    _ -> runMaybeT $ do
      Left (DString d) <- lift $ checkMethode $ maybe konfiguration id konfiguration'
      let hinweis :: Html
          hinweis = specialize sprache $ render $ xmlStringToOutput d
      return $ hinweis ! class_ "alert-danger"
  defaultLayout $ do
    $(widgetFile "aufgabeKonfiguration")

checkKonfiguration :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler (Either Description (Signed (Task, Config)))
checkKonfiguration server aufgabeTyp konfiguration = do
  sprache <- getBevorzugteSprache
  lift $ verify_task_config_localized (unpack server) (unpack aufgabeTyp) (CString $ unpack $ konfiguration) sprache

konfigurationForm :: ToMarkup t => (Text -> Handler (Either Description b)) -> t -> Maybe Text -> AForm Handler Text
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
